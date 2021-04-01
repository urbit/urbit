import React from "react";
import {
  ManagedRadioButtonField as Radio,
  Col,
  Label,
  Text,
} from "@tlon/indigo-react";
import { PropFunc } from "~/types";

export type GroupFeedPermissions = "everyone" | "host" | "admins";

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
        id="everyone"
        label="Everyone"
        caption="Everyone in this group can post and edit this feed"
      />
      <Radio
        name={id}
        id="host"
        label="Host Only"
        caption="Only the host can post and edit this feed"
      />
      <Radio
        name={id}
        id="admins"
        label="Host & Admins Only"
        caption="Only Hosts and Admins can post and edit this feed"
      />
    </Col>
  );
}
