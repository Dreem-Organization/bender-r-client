# Bender Client for Python

> :warning: The full **DOCUMENTATION** on bender-r-client can be found [HERE](https://bender-optimizer.readthedocs.io/en/latest/documentation/r.html).

## Setup

 1. Create an account for free at [bender.dreem.com](https://bender.dreem.com)
 2. Install the Bender R Client package with ``` devtools::install_github("Dreem-Organization/bender-r-client") ```

## Usage Example

> Assuming 'my_optimization_problem()' is your function trying to optimize a result based on your own data and some hyperparameters.

```r
bender = Bender$new("MyBenderMail", "MySecretPassword")
bender$set_algo(id="88155b59-aca1-4cb1-898c-25d942c02859")

suggested_hyperparameters <- bender$suggest(metric="accuracy")
results <- my_optimization_problem(my_data, suggested_hyperparameters)
bender$create_trial(list(accuracy=results.accuracy), suggested_hyperparameters)
```
