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
The implemented algorithm is designed to schedule academic MsC dissertation defenses (vivas).

The intersectAvailabilities function serves to find overlapping availabilities of resources required for a viva, ensuring the availability of resources for scheduling. It begins by extracting the IDs of required resources from the viva, fetching their availabilities, and sorting them chronologically. The function then calculates the duration of the viva and identifies overlapping time slots among the availabilities that meet or exceed this duration. It employs nested loops to compare each pair of availabilities for overlap, considering start and end times, and combines overlapping slots while summing their preferences. The error handling is incorporated to return a DomainError if no common availability slot is found.

The findEarliestTimeSlot function is responsible for determining the earliest available time slot for a given viva, since the algorithm for this phase is First Come First Serve (FCFS). This function operates on a set of intersected availabilities of the required resources, teachers and externals, obtained through the intersectAvailabilities function. After retrieving the duration of the viva from the agenda, it filters the intersected availabilities to identify those that are suitable for accommodating the viva, ensuring that their end time allows for the viva duration. Leveraging an implicit ordering based on availability start times, it then identifies the earliest available time slot among the suitable availabilities. Finally, it adjusts the end time of this slot to accommodate the viva duration, producing the modified earliest time slot.

The scheduleViva function is responsible for the scheduling process for a viva by calling the findEarliestTimeSlot function to identify the earliest available time slot that can accommodate the viva. Once the time slot is determined, the function proceeds to update the availabilities of the required resources to reflect the scheduling of the viva. This involves adjusting the availability periods based on the duration of the viva and the selected time slot. The function carefully handles various scenarios, such as overlapping availabilities and ensuring that the adjustments made do not violate any constraints. Additionally, it calculates and preserves the preference of the viva, so that the schedule can have the total sum of preferences (it does not take into account for the selected availability).

The scheduleAllVivas function schedules multiple vivas using foldLeft operation over the list of vivas. This functional approach allows for a concise and expressive implementation, iterating over each viva and progressively accumulating the results. At each iteration, it invokes the scheduleViva function to find the earliest time slot for the current viva and update the availabilities of the required resources accordingly. The choice of foldLeft facilitates a functional programming paradigm, promoting immutability and composability while simplifying the logic for processing multiple elements.

The scheduleVivas function defines schedules for all vivas based on input data provided in a XML file. It handles each step of the scheduling procedure while addressing potential errors that may occur during data retrieval or scheduling. Initially, it extracts essential information such as the agenda duration, vivas, teachers, externals, and preference from the XML by utilizing pattern matching and error handling. Once the necessary data is retrieved, the function invokes the scheduleAllVivas function to schedule all vivas efficiently. It then sorts the scheduled vivas list by the start time to ensure a coherent representation of the schedule. The choice of sorting the schedule by start date is to match the XML test files. Finally, the function builds the XML element representing the schedule, incorporating details of each scheduled viva along with their corresponding attributes, following the schema defined.

The updateAgenda function saves the updated agenda to an XML file. It pretty much follows the same approach as the scheduleVivas function, but instead, from all the scheduled vivas, it gets information about both vivas and resources and updates the availabilities according to each resource new schedule possibilities.


## Tests
Several unit tests for different components of the scheduling system were written to cover domain errors, XML processing, file I/O operations, simple types validation, and XML to domain conversion. For domain errors, each error type is tested to ensure it returns the correct string. XML processing tests check the correct extraction of nodes and attributes from XML, and the traversal of XML elements. File I/O tests verify the loading of XML files and error messages from files. Simple types tests validate the conversion from strings to domain-specific types like agendaDuration, vivaStudent, vivaTitle, etc. XML to domain tests ensure the correct conversion of XML data to domain objects like Viva, Teacher, and External. The tests are designed to ensure that each component of the system works as expected and handles errors correctly.