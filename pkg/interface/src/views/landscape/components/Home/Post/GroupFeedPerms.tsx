import React from "react";
import {
  ManagedRadioButtonField as Radio,
  Col,
  Label,
  Text,
} from "@tlon/indigo-react";
import { PropFunc } from "~/types";

export function GroupFeedPermsInput(
  props: {
    id: string;
  } & PropFunc<typeof Col>
) {
  const { id, ...rest } = props;

  return (
    <Col gapY="4" {...rest}>
      <Text fontWeight="medium">Permissions</Text>
      <Radio
        name={id}
        id=" "
        label="Everyone"
        caption="Everyone in this group can post and edit this feed"
      />
      <Radio
        name={id}
        id="host-feed"
        label="Host Only"
        caption="Only the host can post this feed. Everyone else may comment"
      />
      <Radio
        name={id}
        id="admin-feed"
        label="Host & Admins Only"
        caption="Only Hosts and Admins can post this feed. Everyone else may comment"
      />
    </Col>
  );
}
