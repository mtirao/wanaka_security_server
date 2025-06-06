import MQTTNIO
import Foundation
import NIOCore

public class WannakaMQTTClient {
    
    private let client: MQTTClient

    public init() {
        client = MQTTClient(
            host: "192.168.0.56",
            port: 1883,
            identifier: "My Client",
            eventLoopGroupProvider: .shared(.singletonMultiThreadedEventLoopGroup)
        )
    }

    public func connect() async throws {
        try await client.connect()
        print("Connected to MQTT broker")
    }

    public func subscribe(to topic: String) async throws {
        let _ = try await client.subscribe(to: [MQTTSubscribeInfo(topicFilter: topic, qos: .atMostOnce)])
        
        print("Subscribed to topic: \(topic)")
    }

    public func publish(to topic: String, message: String) async throws {
        let payload: ByteBuffer = ByteBuffer(string: message)
        try await client.publish(to: topic, payload: payload, qos: .atMostOnce)
        print("Published message to topic \(topic): \(message)")
    }
    
}

