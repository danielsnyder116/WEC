library(googledrive)
library(googlesheets4)
library(cronR)
library(dplyr)

#--------------------------
#     VOLUNTEER DATA
#--------------------------

#INITIAL INTAKE INFO
url_address <- "https://docs.google.com/spreadsheets/d/1u9QMnrfzPAZjMOvFBatvq5HJPRr_LyByzdfKDeZLIn0/edit#gid=1089355021"

#First Sheet: Responses
df <- read_sheet(url_address, sheet="Form Responses 1", col_names = as.character(1:95))


#Getting questions to convert to good column names
orig_col_names <- df %>% slice(1) %>% as.list()

#Getting rid of first row with questions
df <- df %>% slice(-1)

print(orig_col_names)

new_col_names <- c('timestamp', 'email_address', 'notes',
                   'placed_as_tutor_teacher','proof_of_vacc_two', 'vol_intention',
                   'full_name', 'student_name_tutor', 'student_notified_indicator',
                   'phone_num', 'returning_indicator', 'previous_role', 
                   'reserved_student_names', 'reregister_ackn_1', 'reregister_ackn_2',
                   'desired_start_date', 'details_multiple_dates', 'want_new_student_role_indicator',
                   'where_hear_wec', 'why_serve', 'past_apply_not_placed',
                   'had_onboard_call_with_staff', 'role_first_choice', 'pref_class_slot', 
                   'avail_am_class_online', 'avail_am_class_person',
                   'avail_pm_class_online', 'avail_pm_class_person',
                   'avail_citizen_class_online', 'class_level_prefs',
                   'pref_solo_indicator', 'co_teach_info',
                   'teacher_format_prefs', 'other_class_sched_info',
                   'tutor_type_pref', 'pref_tutor_slot', 
                   'avail_am_tutor', 'avail_pm_tutor', 'avail_wknd_tutor',
                   'tutor_level_prefs', 'num_new_students', 'avail_before_sep_27_indicator',
                   'other_tutor_sched_info', 'pref_role_2'
                   
                   
                   
                   
                   
                   
                   
                   )

#------------------------------------------------------------------------

#CURRENT ROSTER


#------------------------------------------------------------------------
