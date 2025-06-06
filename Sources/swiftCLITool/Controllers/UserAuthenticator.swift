import Vapor
import Fluent
import JWT

struct UserAuthenticator: AsyncBasicAuthenticator {
    
    func authenticate(
        basic: BasicAuthorization,
        for request: Request
    ) async throws {
        
        let tenants: [Tenant] = try await Tenant.query(on: request.db)
            .filter(\Tenant.$userName == basic.username)
            .filter(\Tenant.$userPassword == basic.password)
            .all()
        
        guard !tenants.isEmpty, let id = tenants.first?.id as? UUID else {
            throw Abort(.unauthorized)
        }
        
        let payload = Payload(
            subject: SubjectClaim(value: id.uuidString),
            expiration: .init(value: .distantFuture),
            isAdmin: true
        )
        let jwt = try await request.jwt.sign(payload)
        request.auth.login(User(jwt: jwt))
        
   }
}
