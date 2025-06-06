import Fluent

struct CreateTenant: AsyncMigration {
    func prepare(on database: Database) async throws {
        do {
            try await database.schema("tenants")
                .id()
                .field("user_name", .string, .required)
                .field("user_password", .string, .required)
                .field("user_id", .string, .required)
                .field("status", .string, .required)
                .field("created_at", .datetime, .required)
                .create()
        }catch {
            print("\(error)")
        }
    }

    func revert(on database: Database) async throws {
        try await database.schema("tenants").delete()
    }
}
