library(plumber)

#* Echo my name
#* @get /myname
function()
{
    return(list(name='Jared'))
}
