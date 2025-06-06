import Vapor
import Fluent
import FluentPostgresDriver
import JWT

// configures your application
public func configure(_ app: Application) async throws {
   
    app.logger.logLevel = .info

    // register routes
    app.databases.use(
        .postgres(
            configuration: .init(
                hostname: "localhost",
                username: "mtirao",
                password: "",   
                database: "wanaka_security",
                tls: .disable
            )
        ),
        as: .psql
    )

    await app.jwt.keys.add(hmac: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9", digestAlgorithm: .sha256)
    
    app.middleware.use(AuthFilter())
    
    app.migrations.add(CreateTenant())
    

    try routes(app)
}
