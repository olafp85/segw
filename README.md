# SAP Gateway Services
Custom frontend for the SEGW transaction. It displays the same open folders as in SEGW.

Example
![Screenshot](/images/screenshot.png)

Functionalities:
1. Function "Print Preview" has been redefined to perform a cache clearance like in the SAP Gateway Client, menu option Metadata » Cleanup Cache
2. Function "Local File..." has been redefined to navigate to the standard SEGW transaction
3. When double clicking a line the system navigates to the implementation class or when there is exactly one method implemented (one checkmark on the line), the implemented method is shown
4. When a checkmark is clicked, the system directly navigates to the implemented method
