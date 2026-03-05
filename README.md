# One American Day
Created by **[Holden Green](https://hgorledeenn.github.io)** in February 2026 <br>
Columbia Journalism School, Data Studio
<br>

![gif-for-readme.gif](gif-for-readme.gif)

<br>

## Contents:
1. [The Project](#the-project)
2. [Data Wrangling and Visualization](#data-wrangling-and-visualization)
3. [Web Design](#web-design)
4. [What I Would Change for Next Time](#what-i-would-change-for-next-time)
5. [header 5](#)

## The Project
This story analyzes data from the 2024 American Community Survey, a topic I found from [this video](https://www.youtube.com/watch?v=04EfRggPL-M) from Chris Spargo. I analyzed the data in R, which included extensive exploratory data analysis and wrangling.

I learned a lot about working with this sort of large survey data and got hands on experience applying and understanding weights to make claims about entire populations based on data from samples.

<br>

## Data Wrangling and Visualization
This project was my first foray into making [waffle charts](/plots_for_site/1_all_bright.png) and I got hands on experience with `geom_waffle`.

<p align="center">
<img src="to_make_into_viz/waffle_bin_11_05:00 AM.png" width=30%>
<img src="to_make_into_viz/waffle_bin_25_12:00 PM.png" width=30%>
<img src="to_make_into_viz/waffle_bin_45_10:00 PM.png" width=30%>
</p>

I found the top 5 activities (by % of people engaged in them) for every 30-minute interval starting at 12:00am. Once I had these results, I decided on groups for the individual activities to make the final visualizations cleaner (*eg. instead of one **computer use** category and one **watching tv** category, I made a **leisure** category*).

I was working with the activity data as reported by survey respondents, which does not fall neatly into 30 minute increments. Because of this, I had to think through how to most accurately group data into my neat bins for analysis and visualization.

I ultimately decided to simply calculate the overlap of each activity for each bin (which included learning the helpful `tidyr::crossing` command to make every combination of my `bins` and `activity` dataframes) and assign the single activity that took up the majority of a given bin to that entire bin for that respondent.

```R
## Creating every combination of activities and bins
activity_bins <- tidyr::crossing(df_act, bins)

activity_bins <- activity_bins %>%
  mutate(
    ## Calculating overlap
    overlap = pmax(0, pmin(end_since_midnight, bin_end) - pmax(start_since_midnight, bin_start))
  ) %>%
  ## Removing any activity not included in a bin
  filter(overlap > 0)


bin_assign <- activity_bins %>%
  group_by(TUCASEID, bin_id) %>%
  ## Assigning the one activity with the most overlap to each bin
  slice_max(overlap, n = 1, with_ties = FALSE) %>%
  ungroup()
```

<br>

## Web Design
I had a lot of fun with the design of the website for this project. Because I was presenting an analysis of time-based data, I knew I wanted to present the findings chronologically.

To add to the visual cues, I assigned simple background colors via css classes like `.midnight` and `.seven-am`.

I used [coolors.co](coolors.co) to check the accessibilty of my color choices (especially as I started using lighter blue shades for midday sections).

I also created the animation of my waffle plots to show how they changed throughout the day, which lent itself nicely to my header and footer sections. I learned about `svh` in css formatting, which allowed me to ensure that my header title card/animation fills the entire screen, no matter the size or orientation of the screen.

<br>

## What I Would Change for Next Time
For future similar projects, I think it might be interesting to add an interactive component to the site (eg. respondents can input an activity they do and when they do it and can see how common it is).

I also was somewhat short on time before the deadline, but I would have loved to do a more in-depth analysis to find even more interesting findings. For instance, maybe a story about the most common times to eat dinner across the US, or presenting the data in the more granular categories it started as.