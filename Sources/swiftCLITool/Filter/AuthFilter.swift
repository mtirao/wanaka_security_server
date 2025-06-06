//
//  AuthMiddleware.swift
//  CommandLineSwiftToolUsesCxx
//
//  Created by Marcos Tirao on 06/06/2025.
//

import Vapor

struct AuthFilter: AsyncMiddleware {
    func respond(to request: Request, chainingTo next: AsyncResponder) async throws -> Response {
        if request.url.string == "/api/login" {
            return try await next.respond(to: request)
        }
        
        let payload = try await request.jwt.verify(as: Payload.self)
        request.headers.add(name: "X-User-Id", value: payload.subject.value.lowercased())
        return try await next.respond(to: request)
    }
}
