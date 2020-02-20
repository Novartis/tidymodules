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
