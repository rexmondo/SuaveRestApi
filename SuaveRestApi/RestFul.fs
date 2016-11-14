namespace SuaveRestApi.Rest

open Newtonsoft.Json
open Newtonsoft.Json.Serialization
open Suave
open Suave.Operators
open Suave.Http
open Suave.Successful

[<AutoOpen>]
module RestFul =
    open Suave.RequestErrors
    open Suave.Filters

    type RestResource<'a> = {
        GetAll: unit -> 'a seq
        Create : 'a -> 'a
        Update: 'a -> 'a option
        Delete : int -> unit
        GetById : int -> 'a option
        UpdateById : int -> 'a -> 'a option
    }

    // 'a -> Webpart
    let JSON v =
        let settings = new JsonSerializerSettings()
        settings.ContractResolver <-
            new CamelCasePropertyNamesContractResolver()

        JsonConvert.SerializeObject(v, settings)
        |> OK
        >=> Writers.setMimeType "application/json; charset=utf-8"
    
    // string -> 'a
    let fromJson<'a> json =
        JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

    // HttpRequest -> 'a
    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString rawForm =
            System.Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> fromJson<'a>

    // string -> RestResource<'a> -> WebPart
    let rest resourceName resource =
        let resourcePath = "/" + resourceName
        let badRequest = BAD_REQUEST "Resource not found"
        let getAll = warbler (fun _ -> resource.GetAll () |> JSON)
        let handleResource requestError = function
            | Some r -> r |> JSON
            | _ -> requestError
        let resourceIdPath = 
            let path = resourcePath + "/%d"
            new PrintfFormat<(int -> string), unit, string, string, int>(path)
        let deleteResourceById id =
            resource.Delete id
            NO_CONTENT
        let getResourceById =
            resource.GetById >> handleResource (NOT_FOUND "Resource not found")
        let updateResourceById id = 
            request (
                getResourceFromReq >> 
                (resource.UpdateById id) >>
                handleResource badRequest
            )

        choose [
            path resourcePath >=> choose [
                GET >=> getAll
                POST >=> request (
                    getResourceFromReq >> 
                    resource.Create >> 
                    JSON
                )
                PUT >=> request (
                    getResourceFromReq >> 
                    resource.Update >> 
                    handleResource badRequest
                )
            ]
            DELETE >=> pathScan resourceIdPath deleteResourceById
            GET >=> pathScan resourceIdPath getResourceById
            PUT >=> pathScan resourceIdPath updateResourceById
        ]