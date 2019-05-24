# Pattern checklist

From <http://camel.apache.org/enterprise-integration-patterns.html>

**Bold** - exists  
Regular - doesn't exist  
*Italic* - questionable  
~~Strikethrough~~ - not necessary

## Messaging systems

1. **Message Channel** - >->
2. **Message** - any type user wants
3. **Pipes and filters** - use translator and filter
4. **Message router** - use router
5. **Message translator** - use translator
6. **Message endpoint** - use mailboxes

## Messaging channels

1. **Point to point channel** - >->
2. **Publish-subscribe channel** - (<>) on mailbox outputs
3. *Dead letter channel* - not really a pattern, should be implemented by user itself? (But I could create a consumer, which calls some func to send the message, and if it fails directs that message to some error mailbox)
4. *Guaranteed delivery* - I think it's guaranteed inside patterns, and outside is not my concern really.
5. *Message bus* - not sure about adding and removing dynamically part, but I feel that is not my concern.

## Message construction

1. **Event message** - so a simple message
2. *Request-reply* - two separate channels should be created, otherwise wrapper for messages needed I think
3. Correlation identifier - would have to create a wrapper type for messages
4. Return address - would have to create a wrapper type for messages

## Message routing

1. **Content based router** - use router
2. **Message filter** - use filter
3. Dynamic router - have to investigate
4. *Recipient list* - statically yes, dynamically not sure
5. **Splitter** - use splitter
6. **Aggregator** - use aggregator
7. Resequencer - would have to create a wrapper type for messages
8. *Composed message processor* - achievable using splitter, router, aggregator and mailbox combining, but maybe a more convenient solution should be proposed?
9. *Scatter-gather* - broadcast + aggregator?
10. Routing slip - would have to create a wrapper type for messages

There are more, but they aren't in the book (the book is quite old though, so I'm not sure if it's a good idea to exclude those)

## Message transformation

Messages can be anything now, so it's up to user using translator

## Messaging endpoints

1. *Messaging mapper* - that's up to user?
2. **Event driven consumer** - attach consumer at the end instead of a mailbox (but that's static, not dynamic)
3. **Polling consumer** - read from mailbox whenever ready
4. **Message dispatcher** - router is asynchronic
5. **Selective consumer** - use filter
6. *Durable subscriber* - is mailbox enough?
7. *Idempotent consumer* - is filter enough?

## System management

1. Control bus - no
2. *Detour* - again, statically yes, dynamically have to investigate
3. *Wire tap* - can easily implement a static one
4. Message history - would have to create a wrapper type for messages
5. Log - would be possible, correct approach unknown

# Recap

Exists:

1. Message Channel (one application communicates with another)
2. Message (exchanging piece of information)
3. Pipes and filters (Processing on a message)
4. Message router (Route message depending on conditions)
5. Message translator (communicate using different data formats)
6. Message endpoint (connect to a messaging channel)
7. Point to point channel (one receiver)
8. Publish-subscribe channel (many receivers)
9. Event message (transmit events)
10. Content based router
11. Message filter
12. Splitter
13. Aggregator
14. Event driven consumer (consume message automatically when available)
15. Polling consumer (consume message when ready)
16. Message dispatcher (coordinate message processing (use router I guess))
17. Selective consumer (selective consumer (use filter I guess))

Needs message wrapper:

1. Request reply
2. Correlation identifier
3. Return address
4. Resequencer
5. Routing slip
6. Message history
7. All "Message transformation" patterns

Dynamic stuff:

1. Message bus (applications easily added or removed?)
2. Dynamic router
3. Recipient list
4. Control bus (administer messaging system)
5. Detour (reroute through additional steps if needed)
6. Wire tap (inspect messages)

Unknown:

1. Dead letter channel (could make a simple static version)
2. Guaranteed delivery
3. Composed message processor (possible, but maybe could be simpler)
4. Scatter gather (possible, but maybe could be simpler)
5. Messaging mapper (that's integration between some system and messaging infrastructure)
6. Durable subscriber (is mailbox keeping messages enough?)
7. Indempotent consumer (is filter enough?)
8. Log (unknown approach)
