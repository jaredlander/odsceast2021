library(tidymodels)

data(credit_data, package='modeldata')

# build a feature engineering recipe
# build a model specification
# combine them into a workflow
# fit a model on the credit data
# save the trained model to disc for deployment later

rec0 <- recipe(
    Status ~ Income + Seniority + Records + Amount + Job, 
    data=credit_data
) %>% 
    step_other(all_nominal(), -Status, other='Misc') %>% 
    step_nzv(all_predictors()) %>% 
    step_dummy(all_nominal(), -Status, one_hot=TRUE)

spec0 <- boost_tree(
    mode='classification',
    trees=200,
    tree_depth=3,
    learn_rate=0.15,
    sample_size=0.65
) %>% 
    set_engine('xgboost', scale_pos_weight=2.55)

flow0 <- workflow() %>% 
    add_recipe(rec0) %>% 
    add_model(spec0)

mod0 <- fit(flow0, data=credit_data)

one_row <- data.frame(Income=156, Seniority=5, Records='yes', Amount=4500, 
                      Job='fixed')
one_row

predict(mod0, new_data=one_row)
predict(mod0, new_data=one_row, type='prob')

readr::write_rds(mod0, file='code/mod0.rds')

loaded_mod <- readr::read_rds('code/mod0.rds')
predict(loaded_mod, new_data=one_row, type='prob')

jsonlite::write_json(x=one_row, path='one_row.json')
