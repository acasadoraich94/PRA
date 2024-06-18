# PRA2 - Visualització de Dades

Aquesta aplicació Shiny ha estat desenvolupada per a la segona part de la pràctica de l'assignatura de Visualització de Dades del Màster Universitari en Ciència de Dades de la UOC. L'objectiu d'aquesta aplicació és visualitzar les dades electorals del Parlament de Catalunya i l'evolució de la intenció de vot dels diferents partits polítics entre 2021 i 2024.
Per accedir-hi entreu a: https://acasadoraich.shinyapps.io/PRA2/

## Descripció

L'aplicació està estructurada en diverses pestanyes:

1. **Presentació**: Una breu introducció a l'aplicació.
2. **Intenció de Vot**: Visualització de l'evolució de la intenció de vot per als diferents partits polítics des del 2021 fins al 2024. Inclou un gràfic de línies i una matriu de correlació per analitzar les relacions entre les variables sociodemogràfiques i la intenció de vot.
3. **Resultats Electorals**: Presentació interactiva dels resultats de les eleccions al Parlament de Catalunya del 12 de maig de 2024. Els usuaris poden veure els percentatges de vot per a cada partit a nivell municipal i aplicar filtres demogràfics.
4. **Codi Font**: Mostra el codi R que genera l’aplicació.

## Instal·lació

Per executar aquesta aplicació localment, segueix els següents passos:

1. Clona aquest repositori:
    ```bash
    git clone https://github.com/acasadoraich94/PRA.git
    ```

2. Instal·la les dependències necessàries. Pots utilitzar el fitxer `renv.lock` per assegurar que totes les versions de paquets són correctes:
    ```r
    install.packages("renv")
    renv::restore()
    ```

3. Executa l'aplicació:
    ```r
    shiny::runApp('C:/Users/UOC 2/OneDrive/Màster Universitari en Ciència de Dades/2023/2n semestre/Visualització de dades/PRA/Part II/PRA')
    ```

## Dades

Les dades utilitzades en aquesta aplicació provenen de diverses fonts:

- **Enquesta del Baròmetre d'Opinió Política**: Administrada pel Centre d'Estudis d'Opinió (CEO) de la Generalitat de Catalunya, cobrint el període de maig 2021 fins a abril 2024.
- **Dades Electorals**: Proporcionades per la Generalitat de Catalunya.
- **Dades de població**: Provenen de l’Idescat i corresponen al padró municipal a 1/1/2023.

## Ús

- A la pestanya **Intenció de Vot**, pots filtrar les dades per rang de dates, sexe, grups d'edat, nivell d'estudis i situació laboral.
- A la pestanya **Resultats Electorals**, pots seleccionar diferents variables i aplicar filtres demogràfics. També pots fer clic als municipis del mapa per obtenir informació detallada.

## Llicència

Aquest projecte està llicenciat sota la [MIT License](LICENSE).

## Autor

Albert Casado Raich

## URL d'accés

Podeu accedir a l'aplicació en aquest [enllaç](https://acasadoraich.shinyapps.io/PRA2/).
