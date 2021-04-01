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

the_mod <- readr::read_rds('mod0.rds')

#* Predict status from individual data
#* @param Income:numeric Income, expected in the hudreds
#* @param Seniority:numeric Seniority, expected in the single digits
#* @param Records:string "yes"/"no"
#* @param Amount:numeric Amount in thousands
#* @param Job:string Type of job, one of "fixed"/"freelance"/"others"/"partime"
#* @get /predict
function(Income, Seniority, Records, Amount, Job)
{
    the_data <- data.frame(
        Income=as.numeric(Income),
        Seniority=as.numeric(Seniority),
        Records=Records,
        Amount=as.numeric(Amount),
        Job=Job
    )
    
    return(
        list(
            predicted_status=predict(the_mod, new_data=the_data, type='prob')
        )
    )
}
