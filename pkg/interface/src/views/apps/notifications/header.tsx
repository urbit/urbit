import React from "react";
import { Text as NormalText, Row, Icon, Rule } from "@tlon/indigo-react";
import f from "lodash/fp";
import _ from "lodash";
import moment from "moment";
import { PropFunc } from "~/types/util";
import { getContactDetails } from "~/logic/lib/util";
import { Associations, Contact, Contacts, Rolodex } from "~/types";

const Text = (props: PropFunc<typeof Text>) => (
  <NormalText fontWeight="500" {...props} />
);

function Author(props: { patp: string; contacts: Contacts; last?: boolean }) {
  const contact: Contact | undefined = props.contacts?.[props.patp];

  const showNickname = !!contact?.nickname;
  const name = contact?.nickname || `~${props.patp}`;

  return (
    <Text mono={!showNickname}>
      {name}
      {!props.last && ", "}
    </Text>
  );
}

export function Header(props: {
  authors: string[];
  archived?: boolean;
  channel?: string;
  group: string;
  contacts: Rolodex;
  description: string;
  moduleIcon?: string;
  time: number;
  read: boolean;
  associations: Associations;
  chat?: boolean;
} & PropFunc<typeof Row> ) {
  const { description, channel, group, moduleIcon, read } = props;
  const contacts = props.contacts[group] || {};

  const authors = _.uniq(props.authors);

  const authorDesc = f.flow(
    f.take(3),
    f.entries,
    f.map(([idx, p]: [string, string]) => {
      const lent = Math.min(3, authors.length);
      const last = lent - 1 === parseInt(idx, 10);
      return <Author key={idx} contacts={contacts} patp={p} last={last} />;
    }),
    (auths) => (
      <React.Fragment>
        {auths}

        {authors.length > 3 &&
          ` and ${authors.length - 3} other${authors.length === 4 ? "" : "s"}`}
      </React.Fragment>
    )
  )(authors);

  const time = moment(props.time).format("HH:mm");
  const groupTitle =
    props.associations.contacts?.[props.group]?.metadata?.title;

  const app = props.chat ? 'chat' : 'graph';
  const channelTitle =
    (channel && props.associations?.[app]?.[channel]?.metadata?.title) ||
    channel;

  return (
    <Row onClick={props.onClick} p="2" flexWrap="wrap" gapX="1" alignItems="center">
      {!props.archived && (
        <Icon
          display="block"
          mr="1"
          icon={read ? "Circle" : "Bullet"}
          color="blue"
        />
      )}
      <Text mr="1" mono>
        {authorDesc}
      </Text>
      <Text mr="1">{description}</Text>
      {!!moduleIcon && <Icon icon={moduleIcon as any} />}
      {!!channel && <Text fontWeight="500">{channelTitle}</Text>}
      <Rule vertical height="12px" />
      {groupTitle &&
         <>
          <Text fontWeight="500">{groupTitle}</Text>
          <Rule vertical height="12px"/>
        </>
      }
      <Text fontWeight="regular" color="lightGray">
        {time}
      </Text>
    </Row>
  );
}
