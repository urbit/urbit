import {
    Col, ManagedRadioButtonField as Radio,

    Text
} from '@tlon/indigo-react';
import React from 'react';
import { PropFunc } from '~/types';

export function GroupFeedPermsInput(
  props: {
    id: string;
  } & PropFunc<typeof Col>
) {
  const { id, ...rest } = props;

  return (
    <Col gapY={4} {...rest}>
      <Text fontWeight="medium">Permissions</Text>
      <Radio
        name={id}
        id=" "
        label="Everyone"
        caption="Everyone in this group can post to this feed"
      />
      <Radio
        name={id}
        id="host-feed"
        label="Host Only"
        caption="Only the host can post to this feed. Everyone else may comment"
      />
      <Radio
        name={id}
        id="admin-feed"
        label="Host & Admins Only"
        caption="Only the host and admins can post to this feed. Everyone else may comment"
      />
    </Col>
  );
}
