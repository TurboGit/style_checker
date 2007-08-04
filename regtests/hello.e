indexing
	description: "A simple class to display a message"
	author: "Pascal Obry"

class
	HELLO
	
feature  --  Output

	output is
		do
			io.print ("My message is simply : ")
			io.print (msg)
			io.print ("%N")
		end
	
feature  --  Modifier
	
	set (str : STRING) is
		do
			msg := str
		end			
		
feature {NONE}  --  Implementation
	
	msg : STRING
	
end
