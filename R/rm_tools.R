##
# Version     Date                 Action
# ----------------------------------
# 0.0.1       31 March 2023     Initial issue transferred from tutorials repository


library(magrittr)
library(dplyr)

RMUSMOD = function(x, y){
  x - y*(x %/% y)
}

NOTES_SHARP = c('C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#', 'A', 'A#', 'B') %>% tolower
NOTES_FLAT  = c('C', 'D_', 'D', 'E_', 'E', 'F', 'G_', 'G', 'A_', 'A', 'B_', 'B') %>% tolower
FLAT2SHARP  = NOTES_SHARP %>% {names(.)<-NOTES_FLAT;.}
SHARP2FLAT  = NOTES_FLAT  %>% {names(.)<-NOTES_SHARP;.}

NOTE_ORDER  <- c(1:12, 2, 4, 7, 9, 11)
names(NOTE_ORDER)<-union(NOTES_FLAT, NOTES_SHARP)

KEY2SCALE = rep('white', 60)
names(KEY2SCALE) = NOTES_FLAT %>% toupper %>%
  gsub(pattern = "_", replacement = 'b') %>%
  c(paste0(.,'m'), paste0(.,7), paste0(., 'm7'), paste0(., 'dim'))

KEY2MODE = rep(3,12) %>% c(rep(1,12), rep(7,12), rep(1, 12), rep(2, 12))
names(KEY2MODE) = names(KEY2SCALE)

KEY2START = rep(NOTES_FLAT, 5)
names(KEY2START) = names(KEY2MODE)

SCALE = list(
  white = c(0, 2, 1, 2, 2, 1, 2, 2),
  harmonic = c(0, 2, 1, 2, 2, 1, 3, 1),
  # chargah = c(0, 2, 1, 3, 1, 1, 3, 1))
  chargah = c(0, 1, 3, 1, 2, 1, 3, 1))

###############################################################################
#' @export
rythm2duration = function(rythm, target_unit = 1/8){
  rsplit = rythm %>% strsplit('_') %>% unlist
  rsplit[1] %>% strsplit('') %>% unlist %>%
    strtoi(32L) %>% {.[.==0]<-32;.} -> duration
  rythm_unit = 1.0/sum(duration)
  rutils::assert(rythm_unit %in% c(1,1/2,1/4,1/8,1/12,1/16,1/24,1/32), "Durations dont fit in a measure!")
  duration = duration*rythm_unit/target_unit
  rutils::assert(sum(duration)*target_unit == 1.0, "Durations dont fit a measure!")
  return(duration)
}

#' @export
cpitch2pitch = function(cpitch, rythm){
  rsplit = rythm %>% strsplit('_') %>% unlist
  isrest = rsplit[2] %>% strsplit('') %>%
    unlist %>% as.integer %>% as.logical %>% {!.}
  nn = length(cpitch)
  rutils::assert(
    sum(!isrest) == nn,
    sprintf("Given rythm %s does not match number of notes in the given cpitch %s!", rythm, paste(cpitch, collapse = ';')))
  pitch = rep('r', nn)
  pitch[!isrest] <- cpitch
  return(pitch)
}

#' @export
pitchDuration2rythm = function(pitch, duration){
  rutils::assert(length(pitch) == length(duration), 'Arguments `pitch` and `duration` have different lengths!')

  BASE32MAP <- c(as.character(1:9), letters[1:22], '0')

  paste(
    BASE32MAP[duration %>% as.numeric %>% {./min(.)}] %>% paste(collapse = ''),
    as.integer(pitch != 'r') %>% paste(collapse = ''),
    sep = '_')
}


# Applies rythm and cpitch to each measure
# input mpr is a Measure Per Row(MPR) dataframe where these columns are expected:
# `cpitch`, `rythm`
# combines cpitch and rythm into pitch and duration that matches the standard
# rmus format for building a standard rmus music dataframe
# This function generates vales in columns `pitch`` and `duration`
# and returns the data frame
# the values in `pitch` and `duration` columns are string character and contain
# pitches and durations of each measure which are concatenated with separator semicolon.
# todo: check with it's equivalent mpr.add_duration() function in mpr_tools.R
# todo: may need to be removed
#' @export
apply_rythms = function(mpr, target_unit = 1/8){
  measures = which(!is.na(mpr$rythm) & !is.na(mpr$cpitch))
  for(i in measures){
    pitchdur = mpr$cpitch[i] %>% strsplit(";") %>% unlist %>%
      cpitchRythm2pitchDuration(mpr$rythm[i], target_unit = target_unit)
    mpr$pitch[i] <- pitchdur$pitch %>% paste(collapse = ";")
    mpr$duration[i] <- pitchdur$duration %>% paste(collapse = ";")
  }
  return(mpr)
}

# todo: check with it's equivalent mpr2rmd() function in mpr_tools.R
#' @export
mpr2rmusdf = function(mpr){
  measures = which(!is.na(mpr$duration) & !is.na(mpr$pitch))
  out = NULL
  for(i in measures){
    out %<>% rbind(data.frame(duration = mpr$duration[i] %>% strsplit(';') %>% unlist,
               pitch = mpr$pitch[i] %>% strsplit(';') %>% unlist,
               measure = i))
  }
  out$note = out$pitch %>% strsplit("[0-9]") %>% unlist
  out$octave = out$pitch %>% gsub(pattern = "[a-z,_#]", replacement = "") %>% unlist
  out$octave[out$octave == 'r'] <- NA
  return(out)
}



#' @export
semitone = function(notes, octaves){
  mults = notes %>% grep(pattern = "[()]")
  if(length(mults) > 0){
    rutils::assert(octaves %>% grep(pattern = "[()]") %>% identical(mults), "todo: write something!")
    lnts = length(notes)
    stns = rep(NA, lnts)
    for(i in mults){
      nts = notes[i] %>% stringr::str_remove_all("[()]") %>% strsplit(":") %>% unlist
      ovs = octaves[i] %>% stringr::str_remove_all("[()]") %>% strsplit(":") %>% unlist
      stns[i] = paste0('(', semitone(nts, ovs) %>% paste(collapse = ':'), ')')
    }
    sings = sequence(lnts) %>% setdiff(mults)
    stns[sings] <- semitone(notes[sings], octaves[sings])
    return(stns)
  }
  return(as.integer(octaves)*12 + NOTE_ORDER[notes] - 1)
}

#' @export
notes_custom = function(sharps = NULL){
  if(!is.null(sharps)){
    out = NOTES_FLAT
    ind = which(NOTES_SHARP %in% paste0(sharps, '#'))
    out[ind] <- NOTES_SHARP[ind]
  } else {
    out = NOTES_FLAT
  }
  return(out)
}

#' @export
semitone2note = function(semitones, sharps = NULL){
  mults = semitones %>% grep(pattern = "[()]")
  sings = length(semitones) %>% sequence %>% setdiff(mults)
  nts   = rep(NA, length(semitones))
  for(i in mults){
    stns   = semitones[i] %>% stringr::str_remove_all("[()]") %>% strsplit(":") %>% unlist
    nts[i] = paste0('(', semitone2note(stns, sharps = sharps) %>% paste(collapse = ':'), ')')
  }
  NOTES_CUSTOM = notes_custom(sharps)
  nts[sings] = NOTES_CUSTOM[RMUSMOD(semitones[sings] %>% as.integer, 12) + 1]
  return(nts)
}

#' @export
semitone2octave = function(semitones){
  mults = semitones %>% grep(pattern = "[()]")
  sings = length(semitones) %>% sequence %>% setdiff(mults)
  ovs   = rep(NA, length(semitones))
  for(i in mults){
    stns   = semitones[i] %>% stringr::str_remove_all("[()]") %>% strsplit(":") %>% unlist
    ovs[i] = paste0('(', semitone2octave(stns %>% as.integer) %>% paste(collapse = ':'), ')')
  }
  ovs[sings] = as.integer(as.integer(semitones[sings])/12)
  return(ovs)
}

#' @export
shift_semitone = function(semitones, halftunes = 2){
  mults = semitones %>% grep(pattern = "[()]")
  sings = length(semitones) %>% sequence %>% setdiff(mults)
  out   = rep(NA, length(semitones))
  for(i in mults){
    stns   = semitones[i] %>% stringr::str_remove_all("[()]") %>% strsplit(":") %>% unlist
    out[i] = paste0('(', shift_semitone(stns %>% as.integer, halftunes = halftunes) %>% paste(collapse = ':'), ')')
  }
  out[sings] = as.integer(semitones[sings]) + halftunes
  return(out)
}

#' @export
shift_note = function(input, halftunes = 2, sharp = T){
  if(length(input)>1){
    return(input %>% sapply(shift_note, halftunes = halftunes, sharp = sharp) %>% unlist)
  }
  ind = which(NOTES_SHARP == input)
  if(length(ind) == 0){
    ind = which(NOTES_FLAT == input)
    rutils::assert(length(ind) > 0, sprintf('Unknown Note! %s', input))
  }

  ind = RMUSMOD(ind + halftunes, 12)
  ind[ind == 0] <- 12
  if(sharp) return(NOTES_SHARP[ind]) else return(NOTES_FLAT[ind])
}

#' @export
chord_triad = function(input, major = T){

  rutils::chif(major, c(0, 4, 3), c(0, 3, 4)) -> nts

  input %>% shift_note(nts %>% cumsum)
}


#' @export
scale_mode_distances = function(scale = 'white', mode = 1){
  distances = SCALE[[scale]]
  c(distances[1], distances[- sequence(mode)], distances[1+sequence(mode-1)])
}

scale_notes = function(scale = 'white', mode = 1, start = "a", sharp = T){
  dst = scale_mode_distances(scale = scale, mode = mode)
  shift_note(start, dst %>% cumsum, sharp = sharp)
}


#' @export
note_octave2function = function(notes, octaves = NULL, scale = 'white', mode = 1, start = "a"){
  lnts  = length(notes)
  mults = notes %>% grep(pattern = "[()]")
  if(length(mults) > 0){
    rutils::assert(octaves %>% grep(pattern = "[()]") %>% identical(mults), "todo: write something!")
    fun_1 = ""
    fun_2 = ""
    all   = sequence(lnts)
    while((length(mults) > 0) | (length(all) > 0)){
      if(length(mults) > 0){
        first = min(mults)
        sings = all[all < first]
      } else {
        sings = all
      }

      if(length(sings) > 0){
        fun_sings = note_octave2function(notes[sings], octaves[sings] %>% as.integer, scale = scale, mode = mode, start = start) %>%
          strsplit("_") %>% unlist
        fun_1 %<>% paste0(fun_sings[1])
        fun_2 %<>% paste0(fun_sings[2])
        all %<>% setdiff(sings)
      }
      if(length(mults) > 0){
        nts = notes[first]   %>% stringr::str_remove_all("[()]") %>% strsplit(":") %>% unlist
        ovs = octaves[first] %>% stringr::str_remove_all("[()]") %>% strsplit(":") %>% unlist
        fun_mults = note_octave2function(nts, ovs %>% as.integer, scale = scale, mode = mode, start = start) %>%
          strsplit('_') %>% unlist
        fun_1 %<>% paste0('(', fun_mults[1], ')')
        fun_2 %<>% paste0('(', fun_mults[2], ')')
        mults %<>% setdiff(first)
        all   %<>% setdiff(first)
      }
    }

    return(paste(fun_1, fun_2, sep = '_'))
  }

  scale_steps_sharp = scale_notes(mode = mode, start = start, sharp = T)
  scale_steps_flat  = scale_notes(mode = mode, start = start, sharp = F)
  func = notes %>% sapply(function(i, sharp, flat) which(i == sharp | i == flat), sharp = scale_steps_sharp[-8], flat = scale_steps_flat[-8]) %>% unlist
  rutils::assert(length(func) == lnts, "Some of the given notes are out of scale!")
  out = func %>% paste(collapse = '')
  if(!is.null(octaves)){
    rutils::assert(length(octaves == lnts), 'Arguments `octave` and `notes` have different lengths!')
    locations = semitone(notes, octaves) - semitone(start, min(octaves)) + 12
    locations = as.integer(locations/12)
    out %<>% paste(paste(locations - min(locations), collapse = ''), sep = '_')
  }
  return(out)
}

#' @export
function2note_octave = function(func, scale = 'white', mode = 1, start = "a", starting_octave = 4){
  if(length(func) > 1){
    return(func %>% lapply(function2note_octave, scale = scale, mode = mode, start = start))
  }

  fsplit = func %>% strsplit('_') %>% unlist

  scale_notes = scale_notes(mode = mode, start = start, sharp = F)

  aa = fsplit[1] %>% strsplit("[()]") %>% unlist
  bb = fsplit[2] %>% strsplit("[()]") %>% unlist

  if(length(aa) > 1){
    rutils::assert(length(aa) == length(bb), "todo: write something!")
    notes = c()
    octaves = c()
    for(i in sequence(length(aa))){
      res = paste(aa[i], bb[i], sep = '_') %>%
        function2note_octave(scale = scale, mode = mode, start = start, starting_octave = starting_octave)
      if(RMUSMOD(i,2) == 0){
        notes   = c(notes, paste0('(', res$note %>% paste(collapse = ':'), ')'))
        octaves = c(octaves, paste0('(', res$octave %>% paste(collapse = ':'), ')'))
      } else {
        notes   = c(notes, res$note)
        octaves = c(octaves, res$octave)
      }
    }
    return(list(note = notes, octave = octaves))
  }

  notes = scale_notes[fsplit[1] %>% strsplit('') %>% unlist %>% as.integer]

  offsets = fsplit[2] %>% strsplit('') %>% unlist %>% as.integer

  #a = semitone(notes, 4) - semitone(start, 4)
  #a[a<0] = a[a<0]+12
  #octaves = starting_octave + as.integer((a + offsets*12)/12)

  octaves = as.integer((- semitone(notes, 4) + semitone(start, 4) - 1 + 12*(offsets+1))/12) %>%
    {.-min(.)} %>% {.+starting_octave}

  return(list(note = notes, octave = octaves))
}

#' @export
pitch2function = function(pitch, ...){
  pitch = pitch[pitch != 'r']
  if(length(pitch) == 0){return('_')}
  notes = pitch %>% strsplit("[0-9]") %>% unlist
  octaves = pitch %>% gsub(pattern = "[a-z,_#]", replacement = "") %>%
    unlist %>% as.integer

  return(note_octave2function(notes, octaves, ...))
}

note_octave2pitch = function(notes, octaves){
  nnn = length(notes)
  rutils::assert(length(octaves) == nnn, "todo: write something!")
  pitches = rep("", nnn)
  mults = grep(notes, pattern = '[()]')
  mults %>% identical(grep(octaves, pattern = '[()]')) %>%
    rutils::assert('todo: write something!')
  sings = sequence(nnn) %>% setdiff(mults)
  notes[mults] %>% tolower %>%
    stringr::str_remove_all('[()]') %>% strsplit(':') %>%
    purrr::map2(
      .y = octaves[mults] %>% stringr::str_remove_all('[()]') %>% strsplit(':'),
      .f = function(u,v) paste0(u,v)) %>%
    lapply(function(u) paste0('(', paste(u, collapse = ':'), ')')) %>%
    unlist -> pitches[mults]
  notes[sings] %>% tolower %>% paste0(octaves[sings]) -> pitches[sings]
  return(pitches)
}


#' @export
function2pitch = function(func, ...){
  paster  = function(u) paste0(note_octave2pitch(u$note, u$octave))
  noctave = function2note_octave(func = func, ...)
  if(length(func)==1){
    return(paster(noctave))
  } else {
    return(noctave %>% lapply(paster))
  }
}

# todo:
# function2pitch


# todo: consider time unit. May not be 8 always, don't hardcode 8
#' @export
is_rmd = function(df){
  is_issue = df %>% group_by(measure, track) %>% summarise(sumdur = sum(duration)) %>%
    pull(sumdur) %>% {.!=8} %>% sum %>% as.logical
  is_issue = is_issue | (is.na(df$octave[df$note != 'r']) %>% sum)
  return(!is_issue)
}

#' @export
midi2rmd = function(midi, unit = 1/8){
  paste_bruckets = function(v, collapse){
    if(length(v) > 1){
      return(paste0('(', paste(v, collapse = collapse), ')'))
    } else {return(v)}
  }

  midi %>% filter(event == "Note On") %>%
    mutate(measure = as.integer(time/1920) + 1,
           semitone = pitch_semitones(pitch),
           ticks =  duration_to_ticks(duration)) %>%
    arrange(time) %>%
    mutate(dirand = ticks/(1920*unit),
           note = semitone2note(semitone),
           octave = semitone2octave(semitone) %>% as.character,
           track = paste(channel, track, sep = '-')) %>%
    select(channel, track, measure, time, ticks, note, octave, pitch, dirand) -> aa

  measures_started_by_time = aa$measure[which(aa$time == 1920*(aa$measure-1))] %>% unique

  aa %>% filter(!measure %in% measures_started_by_time) %>%
    distinct(channel, track, measure) %>%
    mutate(time = (measure-1)*1920, ticks = 0,
           note = NA, octave = NA, pitch = NA, dirand = "") -> bb

  aa %>% rbind(bb) %>% arrange(time) %>%
    group_by(channel, track, measure, time) %>%
    summarise(duration = unique(dirand) %>% paste_bruckets(collapse = ':'),
              ticks = first(ticks),
              nud = length(unique(dirand)),
              note = paste_bruckets(note, collapse = ':'),
              octave = paste_bruckets(octave, collapse = ':'),
              pitch = paste_bruckets(pitch, collapse = ':')) %>%
    mutate(time_next = time) %>%
    rutils::column.shift.up(keep.rows = T, col = 'time_next') -> cc

  cc$time_next[nrow(cc)] = cc$measure[nrow(cc)]*1920
  cc %>%
    mutate(ticks = ifelse(time_next - time < ticks, time_next - time, ticks)) %>%
    mutate(next_time_expected = time + ticks) %>%
    mutate(time_offset = time_next - next_time_expected) %>%
    select(channel, track, measure, time, time_next, ticks, next_time_expected, time_offset, note, octave, duration, nud, pitch) -> dd

  if(sum(dd$nud != 1) == 0){
    dd$duration = dd$ticks/(1920*unit)
  } else {stop('Handling multiple durations for simultaneous notes is not supported yet!')}

  dd %>% filter(time_offset > 0) %>%
    select(-time, -ticks, -duration, -note, -octave, -pitch, -nud, -time_next) %>%
    rename(time = next_time_expected, ticks = time_offset) %>%
    mutate(note = 'r', pitch = 'r', octave = NA, duration = ticks/(1920*unit)) %>%
    select(channel, track, measure, time, ticks, note, octave, duration, pitch) -> ee

  dd %>% select(channel, track, measure, time, ticks, note, octave, duration, pitch) %>%
    rbind(ee) %>%
    arrange(time) %>%
    filter(ticks > 0) -> ff

  return(ff)
}
