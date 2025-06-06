import Vapor


struct Authenticationontroller: RouteCollection {
    func boot(routes: RoutesBuilder) throws {
        let protected = app.grouped(UserAuthenticator())
        
        protected.get("api", "login") { req -> [String:String] in
            try ["token": req.auth.require(User.self).jwt]
        }
    }
    
}
