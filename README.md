# BGSE Dashboard Project: predict 'em all
BGSE 16-17 Data Warehousing/Computing Lab Project for PokemonGo prediction
Group Members - Daniel Bestard, Michael Cameron, Akhil Lohia

### Overview

The objectives of this project are as follows:

- Study the spawnings of pokemons in Pokemon GO by allowing filtering using the Continent/Country/Type/Pokemon by using maps
- Provide predictive analytics which answer the 2 specific questions that we are studying in the project:
  * Given my location of interest, what pokemon type is most likely to appear at those coordinates?
  * Given my pokemon type of interest, what kind of areas should I go to, to increase my probability of catching that pokemon type.

### Structure

The setup structure of the app is given in the 'steps' file. The basic idea is as follows:

- The data is stored in a MySQL database on the instance
- `prep_data.R` gets the data from the db, does cleaning and saves the resulting R Environment
- `Predictions.R` loads this saved R Environment and does all predictive models
- `app.R` is the controller shiny app which interacts with the user

### Assumption

It is important to note that we make a basic assumption (given the nature of the game), which is as follows:

The probability of at least 1 pokemon appearing at a given location where the user is playing or interested to know about, is 1.

This is because the nature of the game is such that everyone who is playing the game definitely finds pokemon wherever they play. Hence, the dataset to assess the probability of pokemon appearing will be really huge. It is practically impossible to be able to collect this data and definitely not a good idea to make an assumption about the probability of ‘any’ pokemon showing up from just the 300,000 observations we have.

Hence, we can assume that the probability of any pokemon appearing at any given location is 1. Thereafter, we try to predict which type of pokemon it would be.


### Functions of the App

- The **Descriptive** tab provides basic descriptive statistics of our dataset based on the options chosen by the user. The user is allowed to choose which continent, country he wants to look at. For the USA, an extra option of state is available. The user is also allowed to choose whether they are interested in looking at the spawns of a particular pokemon or a full type of pokemons.

- The **Who Will I Battle** tab provides the predicted type of pokemon that would appear at a location selected by the user by choosing latitude and longitude.

- The **Where are you Pikachu?** tab provides the other aim of the project. For a type of pokemon chosen from the list of all, it shows the plots of the 4 most important variables which affect predicting this type of pokemon. That gives an idea about what variables should be kept in mind when hunting for that type of pokemons.

### Implementation

- The 2nd tab predicts the type of pokemon who would appear at given coordinates. This is done using the K-Nearest-Neighbours method where the appropriate K is chosen through cross-validation.
- The 3rd tab predicts the values/variables which are important for a chosen type of pokemon. These important variables have been chosen by running a random forest model, which tells us the most important variables that identify a particular type/class.

### Required packages

The `R` analysis relies on the following packages/files:

- `shiny`
- `dplyr`
- `RMySQL`
- `randomForest`
- `plotly`
- `leaflet`
- 'Predictions.R'

### Languages Used
- R
- SQL
- HTML/CSS
- Javascript
