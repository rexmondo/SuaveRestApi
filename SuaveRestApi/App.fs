open SuaveRestApi.Rest
open SuaveRestApi.Db
open Suave.Web
open Suave.Successful

[<EntryPoint>]
let main argv = 
    let personWebPart = rest "people" {
        GetAll = Db.getPeople
        Create = Db.createPerson
        Update = Db.updatePerson
        Delete = Db.deletePerson
        GetById = Db.getPerson
        UpdateById = Db.updatePersonById
        IsExists = Db.isPersonExists
    }
    startWebServer defaultConfig personWebPart

    0
