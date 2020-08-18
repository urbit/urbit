import React, { Component } from "react";
import { Col, Box } from "@tlon/indigo-react";
import { Link } from "react-router-dom";
import {Note} from "../../../../types/publish-update";

function NavigationItem(props: {
  url: string;
  title: string;
  date: string;
  prev?: boolean;
}) {
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
        <Box color="gray">{props.date}</Box>
      </Link>
    </Box>
  );
}


interface NoteNavigationProps {
  book: string;
  next: any;
  prev: any;
  ship: string;
}
  

export function NoteNavigation(props: NoteNavigationProps) {
  let nextComponent = <div />;
  let prevComponent = <div />;
  let nextUrl = "";
  let prevUrl = "";
  const { next, prev } = props;

  if(!next && !prev) {
    return null;
  }

  if (next) {
    nextUrl = `/~publish/notebook/${props.ship}/${props.book}/note/${props.next.id}`;
    nextComponent = (
      <NavigationItem title={next.title} date={next.date} url={nextUrl} />
    );
  }
  if (prev) {
    prevUrl = `/~publish/notebook/${props.ship}/${props.book}/note/${props.prev.id}`;

    prevComponent = (
      <NavigationItem title={prev.title} date={prev.date} url={prevUrl} prev />
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
