import React, { ReactElement } from 'react';
import moment from 'moment';
import { Link } from 'react-router-dom';
import { BigInteger } from 'big-integer';

import { Box, Text } from '@tlon/indigo-react';
import { Graph } from '@urbit/api';

import { getLatestRevision } from '~/logic/lib/publish';
import Timestamp from '~/views/components/Timestamp';

function NavigationItem(props: {
  url: string;
  title: string;
  date: number;
  prev?: boolean;
}): ReactElement {
  return (
    <Box
      justifySelf={props.prev ? 'start' : 'end'}
      display="flex"
      flexDirection="column"
      justifyContent="flex-end"
      textAlign={props.prev ? 'left' : 'right'}
    >
      <Link to={props.url}>
        <Text display='block' color="gray">
          {props.prev ? 'Previous' : 'Next'}
        </Text>
        <Text display='block' lineHeight="tall">{props.title}</Text>
        <Timestamp
          stamp={moment(props.date)}
          time={false}
          fontSize="1"
          justifyContent={props.prev ? 'flex-start' : 'flex-end'}
        />
      </Link>
    </Box>
  );
}

function getAdjacentId(
  graph: Graph,
  child: BigInteger,
  backwards = false
): BigInteger | null {
  const children = Array.from(graph);
  const i = children.findIndex(([index]) => index.eq(child));
  const target = children[backwards ? i + 1 : i - 1];
  return target?.[0] || null;
}

function makeNoteUrl(noteId: number) {
  return noteId.toString();
}

interface NoteNavigationProps {
  noteId: number;
  notebook: Graph;
  baseUrl: string;
}

export function NoteNavigation(props: NoteNavigationProps): ReactElement {
  let nextComponent = <Box />;
  let prevComponent = <Box />;
  const { noteId, notebook } = props;
  if (!notebook) {
    return null;
  }
  const nextId = getAdjacentId(notebook, noteId);
  const prevId = getAdjacentId(notebook, noteId, true);
  const next = nextId && notebook.get(nextId);
  const prev = prevId && notebook.get(prevId);

  if (!next && !prev) {
    return null;
  }

  if (next && nextId) {
    const nextUrl = makeNoteUrl(nextId);
    const [, title, , post] = getLatestRevision(next);
    const date = post['time-sent'];
    nextComponent = <NavigationItem title={title} date={date} url={nextUrl} />;
  }
  if (prev && prevId) {
    const prevUrl = makeNoteUrl(prevId);
    const [, title, , post] = getLatestRevision(prev);
    const date = post['time-sent'];
    prevComponent = (
      <NavigationItem title={title} date={date} url={prevUrl} prev />
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
