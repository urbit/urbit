import React from 'react';
import moment from 'moment';

import { Box, Col, Row, BaseImage, Text, Icon } from '@tlon/indigo-react';
import { Sigil } from '~/logic/lib/sigil';
import useLocalState from '~/logic/state/local';
import useSettingsState, {selectCalmState} from "~/logic/state/settings";
import {
  uxToHex
} from '~/logic/lib/util';

export const DATESTAMP_FORMAT = '[~]YYYY.M.D';


export function PostHeader(props) {
  const { post, contacts } = props;
  const contact = contacts[`~${post.author}`];
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
        ship={post.author}
        size={36}
        color={color}
        classes={sigilClass}
        icon
        padding={8}
        borderRadius={2}
      />
    );

    const timestamp = moment
      .unix(post['time-sent'] / 1000)
      .format('h:mm A');

  return (
    <Row width="100%" height="36px" mb={3} justifyContent="space-between">
      <Row>
        <Col>
          <Box width="36px" height="36px" mr={2}>{img}</Box>
        </Col>
        <Col>
          <Text mono>~{post.author}</Text>
          <Text gray>{timestamp}</Text>
        </Col>
      </Row>
      <Icon icon="Ellipsis" color="gray" />
    </Row>
  );
}

