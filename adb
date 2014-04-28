#! /bin/bash

ADBDIR=$(readlink -f $(dirname $_))

directorify() {
  echo -n ${1:0:2}/${1:0:4}/${1}
}

ls-ids() {
  find id \! -name content \( -type f -o -type l \) -printf "%f\n"
}

ls-content() {
  find content \! -name content \( -type f -o -type l \) -printf "%h: %f\n" | cut -d/ -f4
}

ls-featids() {
  find features \( -type f -o -type l \) -a \! -name '*~' -printf "%f\n"
}

ls-features() {
  root=extract${1:+/$1}
  find $root \( -type f -o -type l \) -printf "%h\n" 2>/dev/null | awk -F/ '{ printf "%s: %s\n", $2, $5 }'
}

status() {
  printf "adb database '%s'\n" "$(git config --file .adb/config --get core.description)"
  nids=$(ls-ids | wc -l)
  ncontent=$(ls-content | wc -l)
  printf "ids: %d\n" $nids
  printf "content: %d\n" $ncontent
  printf "known features:\n"
  widest=0
  for feature in $(ls-featids)
  do
    width=$(echo -n $feature | wc -c)
    if [ $width -gt $widest ]; then
      widest=$width
    fi
  done
  for feature in $(ls-featids)
  do
    printf "%*s: %d\n" $(($widest+2)) "$feature" $(ls-features $feature | wc -l)
  done
}

add() {
  for file in "$@"
  do
    id=$(metaflac --show-tag MUSICBRAINZ_TRACKID "${file}" | cut -d= -f2)
    if [ -z "$id" ]; then
      printf "missing id for %s" "${file}" 1>&2
      exit 1
    fi
    # FIXME: check for trackid multiply set
    stream5sum=$(metaflac --show-md5sum "${file}")
    if [ "$stream5sum" = "00000000000000000000000000000000" ]; then
      printf "md5sum unset for %s" "${file}" 1>&2
      exit 1
    fi

    mkdir -p id/$(directorify $id)
    ln -sf "${file}" id/$(directorify $id)/${id}
    mkdir -p content/$(directorify $stream5sum)
    ln -sf "${file}" content/$(directorify $stream5sum)/${id}
    # FIXME: most recently added wins
    ln -sf "${file}" content/$(directorify $stream5sum)/content
    ln -sf content/$(directorify $stream5sum)/${id} id/$(directorify $id)/content
  done
}

add-features() {
  for file in "$@"
  do
    cp "${file}" features/
  done
}

feature() {
  feature="$1"
  if [ -z features/"${feature}" ]
  then
    printf "can't tell anything about empty feature\n"
    exit 1
  fi
  case $2 in
      step)
          roqet -q -r csv -i sparql -e 'PREFIX vamp: <http://purl.org/ontology/vamp/> SELECT ?o where { ?s vamp:step_size ?o } ' -D features/"${feature}" | tail -1 ;;
      *)
          printf "unrecognized feature command: $2"
          exit 1 ;;
  esac
}

content() {
  id="$1"
  echo $(basename $(dirname $(readlink id/$(directorify $id)/content)) /)
}

extract() {
  set -x
  feature="$1"
  if [ -z "${feature}" ]
  then
    printf "can't extract for empty feature"
  elif [ ! -e "features/${feature}" ]
  then
    printf "can't extract for unknown feature: %s" "${feature}"
  else
    for id in $(ls-ids); do
      content=$(content "$id")
      mkdir -p extract/$feature/$(directorify $content)
      if [ -f extract/$feature/$(directorify $content)/feature.csv ]; then
        echo "skipping extraction for $id (already done)"
      else
        ~/src/misc/sonic-annotator/sonic-annotator -t features/$feature -w csv --csv-stdout content/$(directorify $content)/content > extract/$feature/$(directorify $content)/feature.csv
      fi
    done
  fi
}

show() {
  if [ -e id/$(directorify "$1")/"$1" ]; then
    content=$(content "$1")
  elif [ -e content/$(directorify "$1")/content ]; then
    content="$1"
  else
    printf "can't find content for %s\n" "$1"
    exit 1
  fi
  feature="$2"
  if [ ! -e features/"$feature" ]; then
    printf "don't know about feature %s\n" "$2"
    exit 1
  fi
  if [ -e audiodb/$feature/$content ]; then
    R --slave -f $ADBDIR/utils/show.R --args audiodb/$feature/$content
  else
    printf "run 'adb audiodbize $feature' first\n"
    exit 1
  fi
}

compare() {
  if [ -e id/$(directorify "$1")/"$1" ]; then
    content1=$(content "$1")
  elif [ -e content/$(directorify "$1")/content ]; then
    content1="$1"
  else
    printf "can't find content for %s\n" "$1"
    exit 1
  fi
  if [ -e id/$(directorify "$2")/"$2" ]; then
    content2=$(content "$2")
  elif [ -e content/$(directorify "$2")/content ]; then
    content2="$2"
  else
    printf "can't find content for %s\n" "$2"
    exit 1
  fi
  feature="$3"
  if [ ! -e features/"$feature" ]; then
    printf "don't know about feature %s\n" "$feature"
    exit 1
  fi
  if [ -e audiodb/$feature/$content1 -a -e audiodb/$feature/$content2 ]; then
    output=$(mktemp)
    R --slave -f $ADBDIR/utils/compare.R --args audiodb/$feature/$content1 audiodb/$feature/$content2 $output
    eog $output
    rm $output
  else
    printf "run 'adb audiodbize $feature' first"
    exit 1
  fi
}


play() {
  if [ -e id/$(directorify "$1")/"$1" ]; then
    command play id/$(directorify "$1")/"$1" "${@:2}"
  elif [ -e content/$(directorify "$1")/content ]; then
    command play content/$(directorify "$1")/content "${@:2}"
  else
    printf "don't know how to play: %s\n" "$1"
    exit 1
  fi
}

describe() {
  if [ -e id/$(directorify "$1")/"$1" ]
  then
    metaflac --show-tag album id/$(directorify "$1")/"$1"
    metaflac --show-tag title id/$(directorify "$1")/"$1"
  elif [ -e content/$(directorify "$1")/content ]
  then
    metaflac --show-tag album content/$(directorify "$1")/content
    metaflac --show-tag title content/$(directorify "$1")/content
  else
    printf "don't know how to describe: %s\n" "$1"
    exit 1
  fi
}

grep() {
  for id in $(ls-ids); do
    if metaflac --export-tags-to - id/$(directorify "$id")/"$id" | command grep "$1" > /dev/null
    then
      echo $id: $(content "$id")
      describe $id
    fi
  done
}

audiodbize() {
  feature="$1"
  if [ ! -e features/"${feature}" ]; then
    printf "unknown feature: %s\n" "${feature}"
    exit 1
  fi
  mkdir -p audiodb/"${feature}"
  for file in $(find extract/"${feature}"/ -name 'feature.csv')
  do
    sbcl --noinform --disable-debugger \
      --load $ADBDIR/utils/csv-parser.lisp \
      --load $ADBDIR/utils/csv2ffte.lisp \
      "${file}" audiodb/"${feature}"/$(basename $(dirname ${file}) /) \
      </dev/null
  done
}

audiodb() {
  feature="$1"
  if [ ! -e features/"${feature}" ]; then
    printf "unknown feature: %s" "${feature}"
    exit 1
  fi
  power="$2"
  if [ -n "$power" -a ! -e features/"${power}" ]; then
    printf "unknown feature: %s" "${power}"
    exit 1
  fi
  dbname="$feature"
  if [ -n "$power" ]; then
    dbname="$dbname"."$power"
    dbstep=$(feature "${feature}" step)
    pstep=$(feature "${power}" step)
    if [ $dbstep != $pstep ]; then
      printf "${feature} and ${power} have different vamp:step_size\n"
      exit 1
    fi
  fi
  mkdir -p audiodb/db
  audioDB -N -d audiodb/db/"$dbname".db
  audioDB -L -d audiodb/db/"$dbname".db
  if [ -n "$power" ]; then
    audioDB -P -d audiodb/db/"$dbname".db
  fi
  for file in audiodb/"$feature"/*
  do
    powerflags=""
    if [ -n "$power" ]; then
      powerflags="-w audiodb/$power/$(basename ${file})"
    fi
    audioDB -I \
      -f "${file}" \
      -k "$(basename ${file})" \
      $powerflags \
      -d audiodb/db/"$dbname".db
  done
}

content() {
  metaflac --show-md5sum id/$(directorify "$1")/"$1"
}

init() {
  if [ -z "$1" ]; then
    printf "usage: adb $0 'description'"
    exit 1
  fi
  mkdir id
  mkdir content
  mkdir features
  mkdir extract
  mkdir .adb
  git config --file .adb/config core.description "$1"
}

command=$1
shift
$command "$@"
