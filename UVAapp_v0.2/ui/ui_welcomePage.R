tabPanel("Home",
         h2(style="text-align:center", 'Welcome'),
         tags$hr(),
         HTML('<center><img src="uva.png" height = 300 width="600"></center>'),
         div(style="text-align:center",
             h1('Ultrasound Visualization & Analysis'),
             h4('Visualise and analyse tongue contours for speech research'),
             tags$hr(),
             HTML('<li>'),
             a('Author: Simon Gonzalez (PhD in Linguistics)', href="https://au.linkedin.com/in/gonzalezsimon", target="_blank"),
             HTML('<li>'),
             a('Email: simon.gonzalez@anu.edu.au', 
               href="https://researchers.anu.edu.au/researchers/gonzalez-ochoa-s",
               target="_blank"),
             HTML('<li>'),
             a('Citation: Gonzalez, S. (2018, September 25). UVA. Retrieved from osf.io/z5hyj'),
             HTML('<li>'),
             a('Online CV', href="https://www.visualcv.com/simongonzalez", target="_blank"),
             HTML('<li>'),
             a('Thesis: Place oppositions in English coronal obstruents: an ultrasound study
                                            - (University of Newcastle, Australia)', 
               href="http://hdl.handle.net/1959.13/1310302", target="_blank")
         )
)