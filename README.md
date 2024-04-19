# Executive Summary

## Authors
- Luís Serapicos -> 1230196
- Telo Gaspar -> 1231304

## Context


This project revolves around the scheduling process involves multiple entities and must adhere to various constraints to ensure optimal timing for each viva. At its essence, the project entails developing two algorithms, using functional programming, that iteratively enhance the scheduling process, aiming to achieve an optimal solution.

## Domain

The domain has three key classes: Viva, Availability, and Resource.

### Viva
The Viva class encapsulates essential information about each viva, including details like the student name, the title of the dissertation, and the roles assigned to resources involved in the viva. By representing roles as a Map with resource IDs as keys, we provide flexibility in assigning various roles to different resources, accommodating the diverse composition of viva juries (President, Advisor, Coadvisor and Supervisor).

### Availability
The Availability class defines time intervals during which a resource is available, specifying start and end times along with a preference value indicating the priority of that interval.

### Resource
The Resource class represents entities of teachers and external persons involved in vivas, with attributes including an ID, a name, and a list of availabilities. By organizing resources with their associated availabilities, we establish a clear structure for managing resource constraints during scheduling. 


## Opaque Types

The attribute types of the classes are defined as opaque types, providing strong type safety and encapsulation of internal representations. Opaque types restrict direct access to the underlying data, allowing only specific operations defined by their interfaces. For instance, vivaStudent, vivaTitle, resourceId, Role, availabilityStart, availabilityEnd, and resourceName are opaque types used to represent viva details. With these types, we ensure that their internal representations are hidden from external manipulation, reducing the risk of unintended data corruption.

For example, the resourceId serves as a fundamental component within the domain model, preventing invalid data by the from method which ensures that only identifiers adhering to specific patterns, such as starting with 'T' (Teacher) or 'E' (External) followed by three digits.


## Domain Errors

The Result[A] type alias represents the outcome of operations within the domain, encapsulating either a successful result of type A (generic) or a domain-specific error captured by the DomainError enumeration, promoting error handling.

The DomainError enumeration defines various types of errors that can occur within the domain, providing a structured approach to error handling. Errors include problems related to IO file operations (IOFileProblem), XML parsing (XMLError), as well as validation errors such as invalid agenda duration, viva student or resource identifiers.


## XML Parsing
Functions were designed to parse XML data into the domain types. They facilitate the transformation of XML representations into meaningful domain objects, ensuring consistency and validity of the data throughout the application.

### for <- yield
In the majority of these functions we used scala's "for yield" because it allows sequential operations while automatically handling error propagation. Each step in the for comprehension is executed sequentially, and any error that occurs at any step can be handled more consistently.

### Pattern matching
Pattern matching is powerful dealing with different cases or conditions like case classes and enums, helping the code to behave as expected because it is type safe.


## Algorithm



## Tests
