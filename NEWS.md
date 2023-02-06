
# tidymodules 0.1.5

- ```obser``` attribute : Now all {tm} modules have an obser attribute which is a list of all server observers. This is a convenient location to explore existing observers and helps in garbage collection. Users need to use ```self$obser$[observer_id]``` as a variable name when initializing the observers. All {tm} examples updated with ```obser```.

- ```destroy``` method : This function destroys the module, it removes all the module's references from the ```ModStore``` (session module and edges) and destroy module observers stored in the ```obser``` attribute mentioned above. Note : This functionality rely on module developers to systematically store observers  in the  ```obser``` list.

- ```suspend``` method : This function suspends module's observers stored in the ```obser``` attribute mentioned above. Note : This functionality rely on module developers to systematically store observers  in the  ```obser``` list.

- ```resume``` method : This function resumes module's observers stored in the ```obser``` attribute mentioned above. Note : This functionality rely on module developers to systematically store observers  in the  ```obser``` list.

# tidymodules 0.1.4

- ```collision``` option : By default {tidymodules} doesn't allow the creation of two modules with same id at the same time (same timestamp). It fails with a collision error. This option which is ```FALSE``` by default, allows the user to disable collision check. This could be useful when users create module in an observer that get triggered twice at the same time.

- ```react``` attribute : Now all {tm} modules have a react attribute which is a list to conveniently store server reactive objects (reactive / reactiveVal / reactiveValues). This list help store module reactive objects and facilitate their access from anywhere within the module object. Users need to use ```self$react$[reactive_id]``` as a variable name when initializing the objects. All {tm} examples updated with ```react```.

- Debug mode with ```TM_DEBUG``` option : display a debug button and highlight module UI. Clicking the button allows to explore module environment in debug mode. try this ```options(TM_DEBUG=TRUE)```.

- Fix issue with parent module look-up function. Now the code takes the parent option provided by the user as the source of truth.

- Fix bug in calculating port length and pipe operators

- Apply styler and fix some issues found with lintr

# tidymodules 0.1.1 -> 0.1.3

- Mainly bug fixes and some improvements here and there in the code

# tidymodules 0.1.0.9007

- Correct port attibutes assignment
- Rename TidyModule field `parent_ports` to `pass_ports`
- Make `assignPort` function work in dynamic context
- Add `inherit` parameter to `addPort` function to better control ports inheritance
- Add extra warnings and exceptions related to nested modules and port inheritance
- Improve module console printing to highlight inherited ports

# tidymodules 0.1.0.9006

- Fix a problem where there is no shiny session argument in app server and calling modules' callModule & callModules.
- Add warning to module get port functions for some specific cases (global vs user session)
- fix module iport & oport functions
- doc fix

# tidymodules 0.1.0.9005

- switch to Apache-2.0 Licence
- fix doc


# tidymodules 0.1.0.9004

- add_module function
- snippets file & function to inject them into RStudio configuration
- new defineEdges() function for parsing module communication instructions

# tidymodules 0.1.0.9003

- Improve how the ports are moved around
- Restrict port assignment to reactive function only. No more reactiveValues as this can be modified by module. tidymodules derived ports are an exception.
- Clean-up pipe operators code
- New '%->>%' pipe
- Move input (i) and ouput (o) ports lists into public field to facilitate port lookup from a module reference
- Add oport/iport to be consistent with the corresponding utility functions
- Add exec`In/Out`put functions
- Fix for Store module when edges are empty
- Add check in ModStore for duplicated edges


# tidymodules 0.1.0.9002

- Adding shiny module code in example 1


# tidymodules 0.1.0.9001

- Support for nested modules stored in parent module attribute list
- Sanitize namespace and group ID when provided
- Switch to shiny getDefaultReactiveDomain to retrieve ShinySession
- Update namespace vignette

# tidymodules 0.1.0.9000

- Add travis-CI for building pkgdsown site
- Remove docs
- Fix & complete docs/vignettes
- Add new TidyModule fields : name & order
- Fix issue for creating nested module in console & setting parent namespace

# tidymodules 0.1.0

- Github release
