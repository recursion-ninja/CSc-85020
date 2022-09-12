### CSc 85020 ST: Theoretical Computer Science (Coding Theory)


#### Overview

Emerging computing systems often operate in regimes where they are prone to failures, high variance in performance, and tight constraints on resources.
This brings up the critical challenge of introducing resource-efficient redundancy at various levels of the system stack.
The domain of information and coding theory provides many valuable tools to address this challenge, and these are increasingly finding their way into modern computer systems.
In this course, we will cover practical tools from information and coding theory relevant to computer systems. Along the way, we will learn how these tools have impacted real-world systems through case studies.
Coding-theoretic approach to computing often leads to insightful tradeoffs and surprising results.
As such, this course should be of interest to both students interested in computer systems/networking and students interested in applied theory.

The course will be structured around lectures by the instructor and paper reading/presentations by the students with open discussions.


#### Course Information

  -   **Term**: Fall 2022
  -   **Instructor**: [Jun Li](https://phantom.cs.qc.cuny.edu/li/)
  -   **Email**: [jun.li@qc.cuny.edu](mailto:jun.li@qc.cuny.edu)
  -   **Schedule**: W 11:45 am - 1:45 pm
  -   **Room**: GradCenter 6421
  -   **Office Hours**: TBD
  -   **Course Website**: [https://phantom.cs.qc.cuny.edu/li/CT](https://phantom.cs.qc.cuny.edu/li/CT) (all general information about the course will be posted here except the materials below)
  -   **Assignments & Grades**: Blackboard
  -   **Announcements & Discussion**: Microsoft Teams


#### Prerequisites

Basic understanding of probability (e.g., conditional probability) and basic understanding of linear algebra (e.g., matrix inverse, transpose) will be assumed.
Basic understanding of computer systems will help in understanding the application context better.


#### Grading Rules

Your grade will be computed based on:

  -   Class presentation (15%)
  -   Research project with intermediate milestones (60%)
  -   Homework (15%)
  -   Class participation (10%)

Your final letters will generally be converted from your grade using the following rules:

  -   A+: &gt;= 95%, A: \[90%, 95%), A-: \[85%, 90%)
  -   B+: \[80%, 85%), B: \[75%, 80%); , B-: \[70%, 75%)
  -   C+: \[66%, 70%), C: \[63%, 66%), C-: \[60%, 63%)
  -   D+: \[56%, 60%), D:\[53%, 56%), D-: \[50%, 53%)
  -   F: &lt; 50%


#### (Tentative) Topics

  -   Introduction to erasure and error-correcting codes and fast implementations
  -   Erasure codes for disk-based storage systems: classical and modern storage codes and system level tradeoffs in erasure-coded storage systems
  -   Codes for newer storage media such as flash devices and system-level tradeoffs in coding for distributed SSD storage systems
  -   Codes for reducing tail latency in distributed caching and machine learning systems
  -   Coded shuffle for distributed computation (e.g., for MapReduce/Spark)
  -   Using machine learning to designing erasure and error-correcting-codes
  -   More topics will be added based on the interest of the class


#### (Tentative) Course Schedule

*Note: topics of future classes are tentative and subject to change.*

  |--------------|----------------------------------------|
  | Date         | Details                                |
  |:-------------|:---------------------------------------|
  | August 31    | Overview Coding Theory (0)             |
  | September 7  | Coding theory (1)                      |
  | September 14 | Coding theory (2)                      |
  | September 21 | Coding theory (3)                      |
  | September 28 | Coding theory (4)                      |
  | October 5    | No class scheduled                     |
  | October 12   | Network coding and its applications    |
  | October 19   | Paper presentation                     |
  | October 26   | Proposal presentation                  |
  | November 2   | Coding for distributed storage systems |
  | November 9   | Paper presentation                     |
  | November 16  | Coded computing                        |
  | November 23  | Paper presentation                     |
  | November 30  | Final presentation                     |
  | December 7   | Final presentation                     |
  | December 14  | Reading Day                            |
  | December 21  | Final Paper Due                        |


#### Policies

**Submission**: Project code and report that are not submitted online before the deadline will be considered late and will be subject to penalty.
The penalty for late submission without a pre-approved extension will be based on the lateness of the submission.
All submitted code should be reasonably documented.
You are expected to submit your files on Blackboard.
If there is something that you would like me to know while grading your assignment, please write it in a file called README and write your message there.
Please do not mail your code to the instructor directly.
You may discuss coursework with other students, but you must write up your coursework independently in your own words.
You are not allowed to search the web for solutions.

**Accessibility**: If you need help due to a registered disability or you think you may have a disability, please talk to me and contact the [Office of Special Services for Students with Disabilities](https://www.gc.cuny.edu/Prospective-Current-Students/Current-Students/Student-Disability-Services#:~:text=Contact%20Us-,Contact%20Us,and%20consult%20the%20Student%20Handbook.) as early as possible.
We will discuss how this course will accommodate your individual learning needs.

**Academic Integrity and Honesty**: Students are expected to adhere to the [CUNY policy on academic integrity](http://www2.cuny.edu/about/administration/offices/legal-affairs/policies-procedures/academic-integrity-policy/).
Questions related to coursework and the academic honesty policy should be directed to the instructor.
A sanction will certainly be imposed on the student committed to any academic fraud.
It varies depending upon the instructor's evaluation of the nature and gravity of the offence.
Possible sanctions include but are not limited to, the following: 
  1.  Require the student to redo the assignment;
  2.  Require the student to complete another assignment;
  3.  Assign a grade of zero to the assignment;
  4.  Assign a final grade of zero for the whole course.

**Recording of Remote Classes**: Students who participate in this class with their camera on or use a profile image are agreeing to have their video or image recorded solely for the purpose of creating a record for students enrolled in the class to refer to, including those enrolled students who are unable to attend live.
If you are unwilling to consent to have your profile or video image recorded, be sure to keep your camera off and do not use a profile image.
Likewise, students who un-mute during class and participate orally are agreeing to have their voices recorded.
If you are not willing to consent to have your voice recorded during class, you will need to keep your mute button activated and communicate exclusively using the "chat" feature, which allows students to type questions and comments live.


#### References

Some materials are adapted from the following related material:

-   [Math 7823: Mathematical Coding Theory](http://math.ucdenver.edu/~wcherowi/courses/m7823/m7823f.html)
-   [15-853: Algorithms in the Real World](https://www.cs.cmu.edu/~15853-f19/)
