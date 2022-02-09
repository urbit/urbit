import { Box, Center, Col, Text } from '@tlon/indigo-react';
import React from 'react';
import useHarkState, { HarkState } from '~/logic/state/hark';
import { harkBinToId, HarkLid, Timebox } from '@urbit/api';
import { Notification } from './notification';

const unseenLid = { unseen: null };
const seenLid = { seen: null };
const selUnseen = (s: HarkState) => s.unseen;
const selSeen = (s: HarkState) => s.seen;
export function NewBox() {
  const seen = useHarkState(selSeen);
  const unseen = useHarkState(selUnseen);
  const empty = Object.keys(seen).length + Object.keys(unseen).length === 0;

  return (
    <Box pt="2" overflowY="auto" overflowX="hidden">
      {empty ? (
        <Center p="3">
          <Text>All clear!</Text>
        </Center>
      ) : (
        <>
          <Lid lid={unseenLid} timebox={unseen} title="Unseen" />
          <Lid lid={seenLid} timebox={seen} title="Seen" />
        </>
      )}
    </Box>
  );
}

function Lid({
  lid,
  timebox,
  title
}: {
  lid: HarkLid;
  timebox: Timebox;
  title: string;
}) {
  if(Object.keys(timebox).length === 0) {
    return null;
  }
  return (
    <>
      <Text gray p="2">
        {title}
      </Text>
      <Col>
        {Object.entries(timebox)
          .sort(([, a], [, b]) => b.time - a.time)
          .map(([binId, n]) => (
            <Notification key={harkBinToId(n.bin)} lid={lid} notification={n} />
          ))}
      </Col>
    </>
  );
}
