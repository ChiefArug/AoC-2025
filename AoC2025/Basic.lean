def hello := "world"
def world := "hello"

infix:arg "s+" => String.append

def together := world s+ hello
#eval together
