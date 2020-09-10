import React, { Component } from "react";
import moment from "moment";
import { Box } from "@tlon/indigo-react";
import { Link } from "react-router-dom";
import {Graph, GraphNode} from "~/types";

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

function getAdjacentId(graph: Graph, child: number, backwards = false): number | null {
  const children = Array.from(graph);
  const i = children.findIndex(([index]) => index === child);
  const target = children[backwards ? i-1 : i+1];
  return target?.[0] || null;
}
  

interface NoteNavigationProps {
  book: string;
  noteId: number;
  ship: string;
  notebook: Graph;
}

export function NoteNavigation(props: NoteNavigationProps) {
  let nextComponent = <Box />;
  let prevComponent = <Box />;
  let nextUrl = "";
  let prevUrl = "";
  const { noteId, notebook } = props;
  if(!notebook) {
    return null;
  }
  const nextId = getAdjacentId(notebook, noteId);
  const prevId = getAdjacentId(notebook, noteId, true);
  const next = nextId && notebook.get(nextId);
  const prev = prevId && notebook.get(prevId);

  if (!next && !prev) {
    return null;
  }

  if (next) {
    nextUrl = `/~publish/notebook/ship/${props.ship}/${props.book}/note/${nextId}`;
    const title = next.post.contents[0]?.text;
    const date = next.post['time-sent'];
    nextComponent = (
      <NavigationItem
        title={title}
        date={date}
        url={nextUrl}
      />
    );
  }
  if (prev) {
    prevUrl = `/~publish/notebook/ship/${props.ship}/${props.book}/note/${prevId}`;
    const title = prev.post.contents[0]?.text;
    const date = prev.post['time-sent'];
    prevComponent = (
      <NavigationItem
        title={title}
        date={date}
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
