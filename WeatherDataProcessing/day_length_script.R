library(BioCro)

year <- '2002' # year of weather data to load

# Set a bunch of default values
def_kick_strength = 0.8
def_clock_gamma = 0.1
def_clock_r0 = 1.5
def_clock_period = 24.0
def_dawn_phase_initial = 200.0
def_dusk_phase_initial = 80.0
def_light_threshold = 60.0
def_solver_method = "Gro_rkck54"
def_output_step_size = 1.0
def_adaptive_error_tol = 1e-6
def_adaptive_max_steps = 200

# Make a function to run the clock
run_biocro_clock <- function(
	kick_strength, clock_gamma, clock_r0, clock_period, dawn_phase_initial, dusk_phase_initial, light_threshold,	# Clock parameters
	weather_data,																									# Environmental conditions
	solver_method, output_step_size, adaptive_error_tol, adaptive_max_steps,										# Solver parameters
	verbose																											# Verbosity
	)
{
	
	# Define the system inputs
	
	poincare_clock_ss_modules <- c("light_from_solar", "oscillator_clock_calculator")
	
	poincare_clock_deriv_modules <- c("night_and_day_trackers", "poincare_clock")

	poincare_clock_initial_state <- list(
		dawn_b = clock_r0 * sin(dawn_phase_initial * pi / 180),
		dawn_a = clock_r0 * cos(dawn_phase_initial * pi / 180),
		dusk_b = clock_r0 * sin(dusk_phase_initial * pi / 180),
		dusk_a = clock_r0 * cos(dusk_phase_initial * pi / 180),
		ref_b = 0.0,
		ref_a = 1.0,
		night_tracker = 1.0,
		day_tracker = 0.0
	)

	poincare_clock_parameters <- list(
		timestep = 1.0,
		kick_strength = kick_strength,
		clock_gamma = clock_gamma,
		clock_r0 = clock_r0,
		clock_period = clock_period,
		light_threshold = light_threshold
	)
	
	poincare_solver <- list(
		type = solver_method,
		output_step_size = output_step_size,
		adaptive_error_tol = adaptive_error_tol,
		adaptive_max_steps = adaptive_max_steps
	)
	
	# Run the simulation
	result <- Gro_solver(poincare_clock_initial_state, poincare_clock_parameters, weather_data, poincare_clock_ss_modules, poincare_clock_deriv_modules, poincare_solver, verbose)
	
	# Return the result
	return(result)
}

# Run the clock
weather_data <- read.csv(file = paste0('./', year, '/', year, '_Bondville_IL.csv'))

result <- run_biocro_clock(
  def_kick_strength, def_clock_gamma, def_clock_r0, def_clock_period, def_dawn_phase_initial, def_dusk_phase_initial, def_light_threshold,	# Clock parameters
  weather_data,																																	# Environmental conditions
  def_solver_method, def_output_step_size, def_adaptive_error_tol, def_adaptive_max_steps,													# Solver parameters
  FALSE																																		# Verbosity
)

weather <- cbind(weather_data, result$day_length)
colnames(weather)[ncol(weather)] <- 'day_length'

# Create directory if necessary and write weather file
create.dir(paste0('./',year,'/'), showWarnings = FALSE)
write.csv(weather, file=paste0('./', year, '/', year, '_Bondville_IL_daylength.csv'))
