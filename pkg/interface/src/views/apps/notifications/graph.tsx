import React, { ReactNode, useCallback } from "react";
import moment from "moment";
import { Row, Box, Col, Text, Anchor, Icon, Action } from "@tlon/indigo-react";
import _ from "lodash";
import {
  Post,
  GraphNotifIndex,
  GraphNotificationContents,
  Associations,
  Content,
  Rolodex,
} from "~/types";
import { Header } from "./header";
import { cite, deSig } from "~/logic/lib/util";
import { Sigil } from "~/logic/lib/sigil";
import RichText from "~/views/components/RichText";
import GlobalApi from "~/logic/api/global";

function getGraphModuleIcon(module: string) {
  if (module === "link") {
    return "Links";
  }
  return _.capitalize(module);
}

function describeNotification(description: string, plural: boolean) {
  switch (description) {
    case "link":
      return `added ${plural ? "new links" : "a new link"} to`;
    case "comment":
      return `left ${plural ? "comments" : "a comment"} on`;
    case "mention":
      return "mentioned you on";
    default:
      return description;
  }
}

const GraphUrl = ({ url, title }) => (
  <Box borderRadius="1" p="2" bg="washedGray">
    <Anchor target="_blank" color="gray" href={url}>
      <Icon verticalAlign="bottom" mr="2" icon="ArrowExternal" />
      {title}
    </Anchor>
  </Box>
);

const GraphNodeContent = ({ contents, module, description, index }) => {
  if (module === "link") {
    const lent = index.slice(1).split("/").length;
    if (lent === 1) {
      const [{ text }, { url }] = contents;
      return <GraphUrl title={text} url={url} />;
    } else if (lent === 2) {
      const [{ text }] = contents;
      return <RichText>{text}</RichText>;
    }
    return null;
  }
  return null;
};
const GraphNode = ({ contents, author, module, description, time, index }) => {
  author = deSig(author);

  const img = (
    <Sigil
      ship={`~${author}`}
      size={16}
      icon
      color={`#000000`}
      classes="mix-blend-diff"
    />
  );

  return (
    <Row gapX="2" py="2">
      <Col>{img}</Col>
      <Col alignItems="flex-start">
        <Row
          mb="2"
          height="16px"
          alignItems="center"
          p="1"
          backgroundColor="white"
        >
          <Text mono title={author}>
            {cite(author)}
          </Text>
          <Text ml="2" gray>
            {moment(time).format("HH:mm")}
          </Text>
        </Row>
        <Row p="1">
          <GraphNodeContent
            contents={contents}
            module={module}
            description={description}
            index={index}
          />
        </Row>
      </Col>
    </Row>
  );
};

export function GraphNotification(props: {
  index: GraphNotifIndex;
  contents: GraphNotificationContents;
  archived: boolean;
  read: boolean;
  time: number;
  timebox: BigInteger;
  associations: Associations;
  contacts: Rolodex;
  api: GlobalApi;
}) {
  const { contents, index, read, time, api, timebox } = props;

  const authors = _.map(contents, "author");
  const { graph, group } = index;
  const icon = getGraphModuleIcon(index.module);
  const desc = describeNotification(index.description, contents.length !== 1);

  const onClick = useCallback(() => {
    if (props.archived) {
      return;
    }

    const func = read ? "unread" : "read";
    return api.hark[func](timebox, { graph: index });
  }, [api, timebox, index, read]);

  return (
    <Col onClick={onClick} p="2">
      <Header
        archived={props.archived}
        time={time}
        read={read}
        authors={authors}
        moduleIcon={icon}
        channel={graph}
        contacts={props.contacts}
        group={group}
        description={desc}
        associations={props.associations}
      />
      <Col pl="5">
        {_.map(contents, (content, idx) => (
          <GraphNode
            author={content.author}
            contents={content.contents}
            module={index.module}
            time={content["time-sent"]}
            description={index.description}
            index={content.index}
          />
        ))}
      </Col>
    </Col>
  );
}

