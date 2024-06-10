# Executive Summary

## Authors
- Luís Serapicos -> 1230196
- Telo Gaspar -> 1231304

## Context


This project revolves around the scheduling process involves multiple entities and must adhere to various constraints to ensure optimal timing for each viva. At its essence, the project entails developing two algorithms, using functional programming, that iteratively enhance the scheduling process, aiming to achieve an optimal solution.

## MS03

### The problem

For this milestone3, we were tasked with implementing a new algorithm to schedule vivas. The new algorithm should consider the preferences of the resources involved in the vivas, aiming to maximize the total preference of the scheduled vivas. The algorithm should prioritize time slots with higher preferences when scheduling vivas, ensuring that the resources' preferences are taken into account during the scheduling process.

### Research

The scheduling of vivas is a complex problem that involves multiple resources and constraints. The scheduling process must consider the availability of resources, the duration of the vivas, and the preferences of the resources involved. By incorporating preferences into the scheduling algorithm, we can optimize the allocation of resources and improve the overall scheduling process.

- Brute Force Algorithm: The brute force algorithm generates all possible combinations of vivas and evaluates the total preference of each combination. It then selects the combination with the highest total preference as the optimal schedule. While this approach guarantees an optimal solution, it is computationally expensive and impractical for large amounts of data (like the XML files provided).

- Greedy Algorithm: With the greedy algorithm, we could iteratively select the time slot that has the maximum preference among all unassigned vivas and availabilities, which can lead to a good solution in many cases. However, it makes the locally optimal choice at each decision point with the hope that these local decisions lead to a global optimum. This means that they can sometimes make decisions that seem best in the short term but are not optimal when considering the entire problem.

- Hopcroft–Karp algorithm: This algorithm is used to find the maximum cardinality matching in a bipartite graph. It could be used to find the maximum preference (weights) matching between vivas and resources, ensuring that the total preference of the scheduled vivas is maximized, but since we are not only interested in the maximum cardinality but also in the total preference, this algorithm would not be the best choice.

- Hungarian Algorithm: The Hungarian Algorithm is used to find the maximum weighted matching in a bipartite graph, a matching that pairs each resource (one set of vertices) with a time slot (the other set of vertices) in a way that maximizes the total preference (the weights on the edges). But the problem is that each resource can have multiple availabilities, and the Hungarian Algorithm only works with one-to-one matching, so it would not be the best choice for our problem.

- Weighted Interval Scheduling Algorithm: This algorithm is used to find the maximum weight subset of intervals that do not overlap. It can be adapted to our problem by considering the preferences of the resources as the weights of the intervals and ensuring that the selected intervals do not overlap. This algorithm could be a good choice for our problem since it considers the preferences of the resources and ensures that the selected time slots do not overlap.


---

## MS02

### Objective

The primary objective of Milestone 2 is to create property-based tests for the viva scheduling problem domain. Property-based testing is a powerful testing methodology that allows us to specify the logical properties our program should always satisfy and then uses these properties to generate test cases. This approach helps us to uncover edge cases that we might not have thought of during manual test case creation.

### Generators

Generators are used to create random data for property-based tests. We have defined generators for the domain classes Viva, Availability, and Resource, as well as for the opaque types used in the domain model. These generators are designed to produce valid instances of the domain classes, ensuring that the generated data adheres to the constraints and patterns defined in the domain model.


#### Generate Three Digit Number
    
The threeDigitNumber generator is used to generate random three-digit numbers, which are commonly used as identifiers for resources and other entities in the domain model. The generator ensures that the generated numbers are within the valid range of 001 to 999, providing a diverse set of identifiers for testing purposes.

#### Generate Resource Id

The resourceId generator is used to create random resource identifiers, which consist of a prefix ('T' for teachers and 'E' for external persons) followed by a three-digit number. The generator ensures that the generated identifiers adhere to the specified pattern, allowing for the creation of valid resource instances for testing.

#### Generate Roles (President, Advisor, Coadvisor, Supervisor)

The role generator is used to create random roles for resources involved in vivas. The generator selects a random role from the predefined set of roles (President, Advisor, Coadvisor, Supervisor), ensuring that the generated roles are valid and representative of the roles assigned to resources in vivas.

#### Generate Unique Resource 

The uniqueResource generator is used to create unique resource instances for testing purposes. The generator combines the resourceId and resourceName generators to create resource instances with unique identifiers and names, ensuring that the generated resources are distinct and representative of real-world data.

### Generate Agenda Duration

The agendaDuration generator is used to create random agenda durations, which represent the time allocated for each viva. The generator generates random durations within the valid range of 1, 1:30 and 2 hours, ensuring that the generated durations are diverse and representative of real-world scenarios.

### Generate Availability Start

The availabilityStart generator is used to create random availability start times for resources. The generator generates random start times within the valid range of 8:00 to 18:00, ensuring that the generated start times are representative of the availability periods of resources.

### Generate Availability End

The availabilityEnd generator is used to create random availability end times for resources. The generator generates random end times within the valid range of 9:00 to 20:00, ensuring that the generated end times are representative of the availability periods of resources.

### Generate Viva Roles

The vivaRoles generator is used to create random role assignments for resources involved in vivas. The generator combines the resourceId and role generators to create role assignments with random resource identifiers and roles.

### Generate Viva

The viva generator is used to create random viva instances for testing purposes. The generator combines the vivaStudent, vivaTitle, and vivaRoles generators to create viva instances with random student names, dissertation titles, and role assignments.

### Generate Intersected Availabilities and Scheduled Viva

The intersectedAvailabilities generator is used to create random intersected availabilities for resources involved in vivas in an early phase of the algorithm so we can check for overlaps between availabilities. 
The scheduledViva generator is used to create random scheduled vivas. Similar to the intersectedAvailabilities generator, it generates a duration, unique resources (teachers and externals), combines all resources, and picks a subset of them. It then generates a list of vivas using these resources and uses the algorithm method to schedule all the vivas, resulting in a list of scheduled vivas, the teachers, the externals, and the duration.


### Properties

The properties we have considered relevant for our domain are:

1. All vivas must be scheduled in the intervals in which its resources are available: This property ensures that a viva is only scheduled when all its required resources are available. It helps to maintain the integrity of the scheduling process by ensuring that no viva is scheduled at a time when any of its required resources is unavailable.
2. One resource cannot be overlapped in two scheduled vivas: This property ensures that a resource is not double-booked. It helps to prevent scheduling conflicts by ensuring that a resource is not assigned to more than one viva at the same time.
3. The generated availability intervals must contain at least one interval equal to or greater than the viva duration: This property ensures that the availability intervals generated for resources contain at least one interval that is equal to or greater than the duration of the viva. It helps to ensure that the scheduling algorithm can find a suitable time slot for the viva within the availability intervals of the resources.
4. The generated resources must have unique identifiers: It helps to prevent conflicts and inconsistencies in the scheduling process by ensuring that each resource is uniquely identified and do not appear more than once in the same viva.
5. The total preference of all scheduled vivas must match the total preference of the agenda: This property ensures that the total preference of all scheduled vivas matches the total preference of the agenda.
6. The generated availability intervals must not overlap each other: This property ensures that the generated availability intervals for resources do not overlap with each other. It helps to prevent scheduling conflicts by ensuring that each resource is available at distinct time intervals.



We consider these properties relevant to our domain as they help to check the scheduling of vivas. They were implemented to catch potential scheduling conflicts and inconsistencies, improving the reliability of the scheduling algorithm.



---


## MS01

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