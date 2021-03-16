import React from 'react';
import moment from 'moment';

import { Box, Col, Row, BaseImage, Text } from '@tlon/indigo-react';
import { Sigil } from '~/logic/lib/sigil';
import useLocalState from '~/logic/state/local';
import useSettingsState, {selectCalmState} from "~/logic/state/settings";
import {
  uxToHex,
  cite,
  writeText,
  useShowNickname,
  useHideAvatar,
  useHovering
} from '~/logic/lib/util';

import { PostContent } from './PostContent';

export const DATESTAMP_FORMAT = '[~]YYYY.M.D';


export function PostHeader(props) {
  const { node } = props;
  const contact = props.contacts[`~${node.post.author}`];
  console.log(contact);
  const osDark = useLocalState((state) => state.dark);
  const theme = useSettingsState(s => s.display.theme);
  const dark = theme === 'dark' || (theme === 'auto' && osDark)
  const { hideAvatars } = useSettingsState(selectCalmState);

  const color =
    contact ? `#${uxToHex(contact.color)}` : dark ? '#000000' : '#FFFFFF';
  const sigilClass =
    contact ? '' : dark ? 'mix-blend-diff' : 'mix-blend-darken';

  const img =
    contact?.avatar && !hideAvatars ? (
      <BaseImage
        display='inline-block'
        style={{ objectFit: 'cover' }}
        src={contact.avatar}
        height={36}
        width={36}
        borderRadius={2}
      />
    ) : (
      <Sigil
        ship={node.post.author}
        size={36}
        color={color}
        classes={sigilClass}
        icon
        padding={8}
        borderRadius={2}
      />
    );

    const timestamp = moment
      .unix(node.post['time-sent'] / 1000)
      .format('h:mm A');

  return (
    <Row width="100%" height="36px" mb={2}>
      <Col>
        <Box width="36px" height="36px" mr={2}>{img}</Box>
      </Col>
      <Col>
        <Text mono>~{node.post.author}</Text>
        <Text gray>{timestamp}</Text>
      </Col>
    </Row>
  );
}
export function PostFooter(props) {
  return <div></div>
}


export function PostItem(props) {
  const { index, node, groups, associations, api, contacts } = props;
  return (
    <Row
      pl="1"
      pr="1"
      mb="3"
      width="100%"
      height="100px"
      justifyContent="center">
      <Box
        p="3"
        border={1}
        borderColor="washedGray"
        height="100px"
        width="100%"
        maxWidth="600px">
        <PostHeader node={node} contacts={contacts} />
        <PostContent {...props} />
        <PostFooter /> 
      </Box>
    </Row>
  );
}

