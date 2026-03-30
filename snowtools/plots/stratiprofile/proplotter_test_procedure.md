# Tests for proplotter

The GUI proplotter have to be manually tested before any commit. Here is a possible way of testing the software.

## Test 1 : overall test of GUI

1. Open the GUI with `proplotter` command
2. Open the file `/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/PRO_gdesRousses_2019-2020.nc` :
    - Click on open button
    - Select the file
    - Click OK
    - Check the filename if well indicated in the status bar and next to the open button
3. Select variable SSA and Temperature
    - Check the plot button become red as soon as one variable is changed
4. Select point
    - Select elevation 2400m
    - Click Plot: should have an error "too much points".
    - Select Aspect 0
    - Select Slope 20
5. Click Plot
    - Check main graph
6. Move the mouse on the graph
    - Check secondary graph
7. A right click should stop animation of right graph
    - Right click to stop animation
    - Right click again should restore the animation
8. Zoom
    - Zoom on right graph
    - Check the left graph still work
    - Right click to stop animation
    - Right click again should restore the animation
9. Unzoom
    - Use previous button in the matplotlib bar to go to the previous zoom
10. Re-zoom
    - Zoom on left graph
11. Change point
    - Change aspect to 180°
    - Check the graphs (left anr right) are updated
12. Unzoom
    - Zoom and then unzoom with the home button
13. Change variable
    - Use variables albedo/SSA
    - Check the two graphs are updated
14. Reset button
    - Click reset
15. Change graph to "Multiple profil"
    - Select variables SNOWTYPE and SNOWTEMP
16. Select group of points
    - Choose elevation 2400, slope 40 and remove aspect value
17. Plot
    - Plot button
    - Use the slicer to go to 2019-12-24
    - Check the slicer correctly print date
    - Go to the next timestep, next day and next month and come back to 2019-12-24
    - Check right graph
    - Right click to stop animation
    - Right click again should restore the animation
18. Open 2D file
    - go back to Standard graph
    - Use `Ctrl + O`
    - Select the `/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/PRO_2014080106_2015010106_grille2d.nc` file.
    - Select SNOWTEMP and SNOWTEMP variables
    - Select first x and y values
    - Plot
    - Check graph
19. Quit
    - Use the upper right cross.

## Test 2 : CLI

1. Open a file
    - `proplotter -v SNOWTYPE -p SNOWTEMP -s ZS=2700 -s aspect=0 -s slope=40 /rd/cenfic3/cenmod/home/viallonl/testbase/PRO/PRO_gdesRousses_2019-2020.nc`
    - Should open point 128
2. Check right and left graphs
3. Quit using ESC key

## Test 3 : Escroc plot

1. Open a file
    - `proplotter -v SNOWSSA -p SNOWTEMP /rd/cenfic3/cenmod/home/viallonl/testbase/PRO/ensemble/mb0002/pro/PRO_2016080106_2017080106.nc`
2. Choose Point nr to be at 0 (it should fill the latitude and longitude values)
3. Change Graph type to be Escroc Profil
4. Plot (the plot is empty)
5. Move the date slicer around the middle of the bar
6. Check left and right graphs
7. Test freezing graph with right click
7. Change Graph type to Escroc Season (the plot disappear)
8. Plot (nothing on the left, right graph = season profil)
9. Check that if you move on the left (slowly), the graph season is changing in the right
10. Move the slicer in order to have something on the left graph
11. Check that if you change bar in the left graph, the right graph is changing (beware to the lag)
12. Test zoom then Quit

## Test 4 : Mapplotter plot

1. Open mapplotter with `python mapplotter.py`
2. Choose file `/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/PRO_2014080106_2015010106_grille2d.nc`
3. Choose snow layer specific surface area for Variable
4. Choose first Number_of_Patches ans third snow_layer
5. Move the slicer nearly at the end (december the 19th)
6. Plot
7. Check the Day and Month functionalities of the slicer (add a day, change one timestep with + ou -)
8. Plot after each change of date
9. Change variable (Total_snow_depth) and Plot then Quit