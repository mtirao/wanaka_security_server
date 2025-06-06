import Vapor

struct User: Authenticatable {
    var jwt: String
}
