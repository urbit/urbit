import React, { Component } from "react";
import moment from "moment";
import { Box } from "@tlon/indigo-react";
import { Link } from "react-router-dom";
import { Notebook } from "../../../../types/publish-update";

function NavigationItem(props: {
  url: string;
  title: string;
  date: number;
  prev?: boolean;
}) {
  const date = moment(props.date).fromNow();
  return (
    <Box
      justifySelf={props.prev ? "start" : "end"}
      display="flex"
      flexDirection="column"
      justifyContent="flex-end"
      textAlign={props.prev ? "left" : "right"}
    >
      <Link to={props.url}>
        <Box color="gray" mb={2}>
          {props.prev ? "Previous" : "Next"}
        </Box>
        <Box mb={1}>{props.title}</Box>
        <Box color="gray">{date}</Box>
      </Link>
    </Box>
  );
}

interface NoteNavigationProps {
  book: string;
  nextId?: string;
  prevId?: string;
  ship: string;
  notebook: Notebook;
}

export function NoteNavigation(props: NoteNavigationProps) {
  let nextComponent = <Box />;
  let prevComponent = <Box />;
  let nextUrl = "";
  let prevUrl = "";
  const { nextId, prevId, notebook } = props;
  const next =
    nextId && nextId in notebook?.notes ? notebook?.notes[nextId] : null;

  const prev =
    prevId && prevId in notebook?.notes ? notebook?.notes[prevId] : null;
  if (!next && !prev) {
    return null;
  }

  if (next) {
    nextUrl = `${props.nextId}`;
    nextComponent = (
      <NavigationItem
        title={next.title}
        date={next["date-created"]}
        url={nextUrl}
      />
    );
  }
  if (prev) {
    prevUrl = `${props.prevId}`;
    prevComponent = (
      <NavigationItem
        title={prev.title}
        date={prev["date-created"]}
        url={prevUrl}
        prev
      />
    );
  }

  return (
    <Box
      px={2}
      borderTop={1}
      borderBottom={1}
      borderColor="washedGray"
      display="grid"
      alignItems="center"
      gridTemplateColumns="1fr 1px 1fr"
      gridTemplateRows="100px"
    >
      {prevComponent}
      <Box borderRight={1} borderColor="washedGray" height="100%" />
      {nextComponent}
    </Box>
  );
}

export default NoteNavigation;
