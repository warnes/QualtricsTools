---
title: "QualtricsTools Shiny App"
subtitle: "Frequently Asked Questions"
author: "Emma Morgan"
date: "5/18/2020"
output: html_document
---



# QualtricsTools Shiny App Frequently Asked Questions

1. **How do I run the QualtricsTools reporting app?**  

    Install the QualtricsTools package from GitHub: `devtools::install_packages("emma-morgan/QualtricsTools")`.
    Once the package is installed, load the QualtricsTools functions by running `library(QualtricsTools)`. 
    Finally, launch the QualtricSTools app by running `app()` from the R command line.

2. **How do I load my survey project to create reports?**

    Creating reports with the QualtricsTools app requires a Qualtrics Suvey File (.qsf) and data exported from Qualtrics (.csv). 
    See the QualtricsTools wiki for information about .qsf and .csv usage requirements and guidelines. 
    Use the File Uploader in the left sidebar to select your survey .qsf and .csv.
    
    You can navigate through your file system to choose the .qsf and .csv files using the "Browse" buttons below each selection note.
    Save yourself some time by entering the path to your project in the text entry box above "Choose QSF Survey File". 
    This will a starting point to browse for the survey .qsf and .csv.
    
3. **Data Export**

    QualtricsTools is configured to work with specific data export formats. For best results, use the 
    Data Table export format. 
    
4. **Can I preview my downloaded reports in the app?**

    Yes! Navigate to "Processed Results" in the left sidebar. 
    You should now see five tabs displayed across the top.
    
    - **results tables**: This is the main part of your survey summary report. This includes 
    tabled results to closed-ended questions and the text of open-ended text questions 
    references to "See Appendix". The survey summary report includes a header with the name 
    of the Qualtrics Survey and the number of responsesin the .csv data file.
    
    - **question dictionary**: The question dictionary provides a summary of question information
    from Qualtrics including the question text and question type. It can provide a useful 
    overview of your data. Check the box for "Only Uncodable Questions" to see which survey 
    questions could not be automatically processed by QualtricsTools.
    
    - **verbtim comment appendices**: Verbatim Comment Text Appendices will include full text 
    (verbatim) comments to open-ended questions. comments are tabled with a header that lists
    the appendix reference, question text, and a note that comments have not been edited 
    in any way.
    
    - **display logic**: Display logic is summarized based on the recorded logic in Qualtrics. 
    This is not often useful for readers, but it does provide a summary for analysts. 
    
        *Pro tip: Add user notes in Qualtrics with text descriptions of display logic. 
        These will be added to your report under the question text and will provide better
        descriptions and context for viewers of your report.*
        
    - **coded comment appendices**: Coded comment appendices provide useful summaries of 
    responses to open-ended questions. QualtricsTools is designed to work with the 
    qualitative data analysis program NVivo. See the QualtricsTools wiki for more information about setting up your report to include coded comment appendix tables. 
    
       Coded comment appendices require additional input to specify a file directory with formatted coded comment export tables. Additionally, analysts have the option to include both verbatim and coded comment tables or create an appendix with only coded comments. To specify options for coded comment appendices, choose "More options" in the left sidebar. Use the box in the third column titled "Comment Coding Options" to specify your coded comment appendices.
       
5. **How do I create Coded Comment Appendices?**
    
    Choose "More Options" from the left sidebar, then navigate to the box for "Comment Coding Optoins". Use the options as specified.
    
    - **Generate Coded Comments?** Select the radio button for "Yes, Generate coded Comment Appendices" to start creating coded comment appendices.
    - **Code Type?** QualtricsTools will process coded comment exports using the current OIR NVivo format and previously used Filemaker Pro export format. See the QualtircsTools Wiki page about coded comment appendices for more information and instructions for formatting your coded comment export tables.
    - **Include Verbatim Comments in Coded Appendices?** Select "Yes" if you would like your coded comment appendices to include both coded and verbatim comment tables. Select "No" to create appendices with only coded comment tables that do not include verbatim comments. The App provides options to include all verbatim comments or no verbatim comments. If you would like to include a subset of verbatim comments, please use the command line functions available in R.
    
    - **Sheets Folder Selector** Use this to select the folder with your coded comment data export files. Click the "..." button. This will launch launch a file explorer window. Use this as usual to navigate to the coded comment sheets directory.
    
6. **My survey has complicated flow and is not processing correctly. What can I do?"**

    QualtricsTools does not play nicely with complicated survey flow. Under "More options" check the box for **Ignore Survey Flow** to process the survey blockss in order. 
    
7. **Can I create reports for subgroups of respondents.**

    Yes! Under "More options" navigate to the middle section for **Splitting Respondents**. From here, you can choose one or more columns to create split reports. This will create survey summary tables and verbatim text appendices for each subgroup specified by the split response columns. Click the button "Download All Split Reports and Appendices" to download tables and appendices for all subgroups. If you would like to download a report for one specific subgroup, choose the target group from the **Split Respondents Group** dropdown menu.
    
8. **Can I specify a name for my download files?**

    Yes! Under "More Options" the first column for "Downloads" has an option for naming your files. This is recommended as a brief name for your survey. Files downloaded using the download buttons in this section will include the specified file name and appropriate file description. 
    
    Example: Specified name "Sample Survey" will yield downloaded files with "Sample Survey_results tables" and "Sample Survey_verbatim_comment_appendices."
        
9. **I want to download the tables and verbatim comments and coded comments. How do I do this?**

    In the left sidebar, choose "download zip". This will download results tables, display logic, question dictionary, and verbatim comment appendices.
    
10. **My survey has some weird questions that I do not want to include in my report. Is there a way to leave these out?**

    The left sidebar has an option for "Include/Exclude Responses". This will show a list of questions with checkboxes on the left side of each. Uncheck a question's box and click "apply" to excluee this question from reporting. You should see this updated in the results table and appendix previews under "Processed Results." To add the question back into reports, just re-check the box and click apply.
    
11. **I have other questions. Who can I ask?**

    If you are interested in further customizing your reports, consider using the R command line. Additional information about QualtricsTools functions is available on the QualtricsTools wiki. 
    
       Package maintenance is overseen by Emma Morgan in the Tufts University Office of Institutional Research (emma.morgan@tufts.edu).
   
   
