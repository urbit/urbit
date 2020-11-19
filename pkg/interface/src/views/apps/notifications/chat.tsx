import React, { useCallback } from "react";
import _ from "lodash";
import { Link } from "react-router-dom";
import GlobalApi from "~/logic/api/global";
import {
  Rolodex,
  Associations,
  ChatNotifIndex,
  ChatNotificationContents,
  Groups,
} from "~/types";
import { BigInteger } from "big-integer";
import { Box, Col } from "@tlon/indigo-react";
import { Header } from "./header";
import { pluralize } from "~/logic/lib/util";
import ChatMessage from "../chat/components/ChatMessage";

function describeNotification(mention: boolean, lent: number) {
  const msg = pluralize("message", lent !== 1);
  if (mention) {
    return `mentioned you in ${msg} in`;
  }
  return `sent ${msg} in`;
}

export function ChatNotification(props: {
  index: ChatNotifIndex;
  contents: ChatNotificationContents;
  archived: boolean;
  read: boolean;
  time: number;
  timebox: BigInteger;
  associations: Associations;
  contacts: Rolodex;
  groups: Groups;
  api: GlobalApi;
  remoteContentPolicy: any;
}) {
  const { index, contents, read, time, api, timebox, remoteContentPolicy } = props;
  const authors = _.map(contents, "author");

  const { chat, mention } = index;
  const association = props?.associations?.chat?.[chat];
  const groupPath = association?.["group-path"];
  const appPath = index?.chat;

  const group = props?.groups?.[groupPath];

  const desc = describeNotification(mention, contents.length);
  const groupContacts = props.contacts[groupPath] || {};

  const onClick = useCallback(() => {
    if (props.archived) {
      return;
    }

    const func = read ? "unread" : "read";
    return api.hark[func](timebox, { chat: index });
  }, [api, timebox, index, read]);

  return (
    <Col onClick={onClick} flexGrow="1" p="2">
      <Header
        chat
        associations={props.associations}
        read={read}
        archived={props.archived}
        time={time}
        authors={authors}
        moduleIcon="Chat"
        channel={chat}
        contacts={props.contacts}
        group={groupPath}
        description={desc}
      />
      <Col pb="3" pl="5">
        {_.map(_.take(contents, 5), (content, idx) => {
          let workspace = groupPath;
          if (workspace === undefined || group?.hidden) {
            workspace = '/home';
          }
          const to = `/~landscape${workspace}/resource/chat${appPath}?msg=${content.number}`;
          return (
            <Link key={idx} to={to}>
              <ChatMessage
                measure={() => {}}
                msg={content}
                isLastRead={false}
                group={group}
                contacts={groupContacts}
                fontSize='0'
                pt='2'
                remoteContentPolicy={remoteContentPolicy}
              />
            </Link>
          );
        })}
        {contents.length > 5 && (
          <Box ml="4" mt="3" mb="2" color="gray" fontSize="14px">
            and {contents.length - 5} other message
            {contents.length > 6 ? "s" : ""}
          </Box>
        )}
      </Col>
    </Col>
  );
}
