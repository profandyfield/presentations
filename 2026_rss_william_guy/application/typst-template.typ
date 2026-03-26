#set page(
  paper: "a4",
  margin: (top: 175pt, bottom: 100pt, left: 60pt, right: 60pt),

  header: context [
    #if counter(page).display() == "1" [
      #align(center)[
      #v(24pt)  // adds space above the logo
      #image("/2026_rss_william_guy/application/images/us_logo.png", width: 25%)
      ]
    ]
  ],
  
  footer: [
  #set text(size: 6pt, fill: rgb("#033803"))
  #set par(leading: 3pt) // removes line spacing
  #line(length: 100%, stroke: 0.5pt + rgb("#555555"))  // horizontal rule
  #v(4pt)  // small gap between line and text
  #grid(
    columns: (1fr, 3fr, 1fr),
    align(left)[],  // empty left cell for balance
    align(center)[
      *Andy Field, AcSS, FHEA, C.Stat., C.Psychol., Ph.D., B.Sc.*\
      Professor of Quantitative Methods\
      School of Psychology, University of Sussex\
      Brighton, BN1 9RH, UK \
      andy.field\@sussex.ac.uk \
      https://profiles.sussex.ac.uk/p9846-andy-field/
    ],
    align(right)[
      #image("/2026_rss_william_guy/application/images/rss_cstat_logo_2019.jpg", width: 3cm)
    ]
  )
]
)


// Two-sided margins (mirrors left/right on even pages)
#set page(margin: (inside: 70pt, outside: 50pt)) // remove if you don't need this

#set text(
  font: "Arial", // change font 
  size: 12pt,
)

#set par(
  leading: 12pt,
  justify: true,
)

// Required by Quarto — renders the document body
#show: doc => doc

$body$
