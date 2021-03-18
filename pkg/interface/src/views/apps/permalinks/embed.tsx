import React, { useCallback, useEffect, useState } from "react";
import {
  parsePermalink,
  GraphPermalink as IGraphPermalink,
} from "~/logic/lib/permalinks";
import {
  Action,
  Box,
  Text,
  BaseAnchor,
  Row,
  Icon,
  Col,
} from "@tlon/indigo-react";
import { GroupLink } from "~/views/components/GroupLink";
import GlobalApi from "~/logic/api/global";
import { getModuleIcon } from "~/logic/lib/util";
import useMetadataState from "~/logic/state/metadata";
import { Association, resourceFromPath } from "@urbit/api";
import { Link } from "react-router-dom";
import useGraphState from "~/logic/state/graph";
import { GraphNodeContent } from "../notifications/graph";
import { TranscludedNode } from "./TranscludedNode";

function GroupPermalink(props: { group: string; api: GlobalApi }) {
  const { group, api } = props;
  return (
    <GroupLink
      resource={group}
      api={api}
      pl="2"
      border="1"
      borderRadius="2"
      borderColor="washedGray"
    />
  );
}

function GraphPermalink(
  props: IGraphPermalink & {
    api: GlobalApi;
    transcluded: number;
    pending?: boolean;
  }
) {
  const { pending, link, graph, group, index, api, transcluded } = props;
  const { ship, name } = resourceFromPath(graph);
  const node = useGraphState(
    useCallback((s) => s.looseNodes?.[`${ship.slice(1)}/${name}`]?.[index], [
      graph,
      index,
    ])
  );
  const [errored, setErrored] = useState(false);
  const association = useMetadataState(
    useCallback((s) => s.associations.graph[graph] as Association | null, [
      graph,
    ])
  );
  useEffect(() => {
    (async () => {
      if (pending) {
        return;
      }
      try {
        await api.graph.getNode(ship, name, index);
      } catch (e) {
        console.log(e);
        setErrored(true);
      }
    })();
  }, [pending, graph, index]);
  const showTransclusion = !!(association && node && transcluded < 1);

  const rowTransclusionStyle = showTransclusion
    ? {
        borderTop: "1",
        borderTopColor: "washedGray",
        my: "1",
      }
    : {};

  return (
    <Col my="1" bg="white" border="1" borderColor="lightGray" borderRadius="2">
      {showTransclusion && (
        <Box p="2">
          <TranscludedNode
            transcluded={transcluded + 1}
            node={node}
            assoc={association!}
          />
        </Box>
      )}
      <Row
        {...rowTransclusionStyle}
        alignItems="center"
        justifyContent="space-between"
        width="100%"
        px="2"
        py="1"
      >
        <Row gapX="2" alignItems="center">
          <Icon
            icon={
              association
                ? (getModuleIcon(association.metadata.module) as any)
                : "Groups"
            }
          />
          <Text lineHeight="20px" mono={!association}>
            {association?.metadata.title ?? graph.slice(6)}
          </Text>
        </Row>
        <Action onClick={() => {}}>Go to link</Action>
      </Row>
    </Col>
  );
}

export function PermalinkEmbed(props: {
  link: string;
  association?: Association;
  api: GlobalApi;
  transcluded: number;
}) {
  const permalink = parsePermalink(props.link);

  if (!permalink) {
    return <BaseAnchor href={props.link}>{props.link}</BaseAnchor>;
  }

  switch (permalink.type) {
    case "group":
      return <GroupPermalink group={permalink.group} api={props.api} />;
    case "graph":
      return (
        <GraphPermalink
          transcluded={props.transcluded}
          {...permalink}
          api={props.api}
        />
      );
  }
}
