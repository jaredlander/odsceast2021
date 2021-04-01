library(plumber)

#* Echo my name
#* @get /myname
function()
{
    return(list(name='Jared'))
}

#* Echo back the input
#* @param msg Message to be printed
#* @get /echo
function(msg='default')
{
    list(msg=glue::glue("The message is '{msg}'."))
}

#* Add the two numbers
#* @param x:numeric First number
#* @param y:numeric Second number
#* @get /add
function(x, y)
{
    return(as.numeric(x) + as.numeric(y))
}
