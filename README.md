# Neurowissenschaft im Computerlab FS22 - Quarto

Website and slides for the course "Neurowissenschaft im Computerlab". This repo is based on [neuroscicomplabFS22][1], but instead of using [distill][2] as publishing system, [Quarto][3] is used.

## Development

First of all, make sure to [install Quarto][4]. Since this course has a strong focus on R, R along with all required packages must be installed as well.

If all the dependencies are installed, you have access to those commands while inside the repo:

**`quarto preview`**

This spins up a development server. A browser tab will open which will reload everytime you change the source code.

**`quarto render`**

This will render the whole site and save the result under `docs`.

### File and directory structure

**`_quarto.yml` file**

Defines the Quarto project as a whole.

**`index.qmd` file**

Defines the landing page.

**`bibliography.bib`**

BibTeX bibliography.

**`assets` directory**

Contains assets, like the logo, graphics and so on.

**`docs` directory**

Contains the rendered website and is served by GitHub Pages.

**`pages` directory**

Contains all articles / web pages. Articles reagarding course administration are located under `admin`, articles reagarding course content are located under `chapters` and exercise related articles are located under `exercises`.

**`slides` directory**

Contains slides for the live sessions.

**`styles` directory**

Contains style sheets which customize the (light and dark) themes.

## License

[![CC-BY](https://i.creativecommons.org/l/by/4.0/88x31.png)](https://creativecommons.org/licenses/by/4.0/)

[1]: https://github.com/kogpsy/neuroscicomplabFS22
[2]: https://pkgs.rstudio.com/distill/
[3]: https://quarto.org/
[4]: https://quarto.org/docs/get-started/
