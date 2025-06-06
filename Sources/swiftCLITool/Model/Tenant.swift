import Foundation
import Fluent


final class Tenant:  Model, @unchecked Sendable {
    typealias IDValue = UUID
    static let schema = "tenants"

    @ID(key: .id)
    var id: UUID?

    @Field(key: "user_name")
    var userName: String

    @Field(key: "user_password")
    var userPassword: String

    @Field(key: "user_id")
    var userId: String

    @Field(key: "status")
    var status: String

    @Timestamp(key: "created_at", on: .create)
    var createdAt: Date?

    init() {
        self.id = nil
        self.userName = ""
        self.userPassword = ""
        self.userId = ""
        self.status = ""
        self.createdAt = nil
    }
    init(id: UUID? = nil, userName: String, userPassword: String, userId: String, status: String, createdAt: Date? = nil) {
        self.id = id
        self.userName = userName
        self.userPassword = userPassword
        self.userId = userId
        self.status = status
        self.createdAt = createdAt
    }
}