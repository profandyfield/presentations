#let sign_off(name, title, sig_image, logo_image) = {
  v(1em)
  text[Yours sincerely]
  v(0.5em)
  image(sig_image, width: 5cm)
  v(0.5em)
  text(weight: "bold")[#name] + linebreak()
  text[#title]
  v(0.5em)
  image(logo_image, width: 5cm)
}
