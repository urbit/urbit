import React, { useCallback, useEffect, useState } from "react";
import {
  parsePermalink,
  GraphPermalink as IGraphPermalink,
  getPermalinkForGraph,
  usePermalinkForGraph,
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
      borderColor="lightGray"
    />
  );
}

function GraphPermalink(
  props: IGraphPermalink & {
    api: GlobalApi;
    transcluded: number;
    pending?: boolean;
    showOurContact?: boolean;
    full?: boolean;
  }
) {
  const { full = false, showOurContact, pending, link, graph, group, index, api, transcluded } = props;
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
      if (pending || !index) {
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
  const permalink = getPermalinkForGraph(group, graph, index);

  return (
    <Col
      width="100%"
      bg="white"
      maxWidth={full ? null : "500px"}
      border={full ? null : "1"}
      borderColor="lightGray"
      borderRadius="2"
      onClick={(e) => { e.stopPropagation(); }}
    >
      {showTransclusion && index && (
        <Box p="2">
          <TranscludedNode
            api={api}
            transcluded={transcluded + 1}
            node={node}
            assoc={association!}
            showOurContact={showOurContact}
          />
        </Box>
      )}
      {!!association ? (
        <PermalinkDetails
          known
          showTransclusion={showTransclusion}
          icon={getModuleIcon(association.metadata.config.graph)}
          title={association.metadata.title}
          permalink={permalink}
        />
      ) : (
        <PermalinkDetails
          icon="Groups"
          title={graph.slice(5)}
          permalink={permalink}
        />
      )}
    </Col>
  );
}

function PermalinkDetails(props: {
  title: string;
  icon: any;
  permalink: string;
  showTransclusion?: boolean;
  known?: boolean;
}) {
  const { title, icon, permalink, known, showTransclusion } = props;
  const rowTransclusionStyle = showTransclusion
    ? {
        borderTop: "1",
        borderTopColor: "lightGray",
        my: "1",
      }
    : {};

  return (
    <Row
      {...rowTransclusionStyle}
      alignItems="center"
      justifyContent="space-between"
      width="100%"
      px="2"
      py="1"
    >
      <Row gapX="2" alignItems="center">
        <Icon icon={icon} />
        <Text lineHeight="20px" mono={!known}>
          {title}
        </Text>
      </Row>
      <Link to={`/perma${permalink.slice(16)}`}>
        <Text color="blue">Go to link</Text>
      </Link>
    </Row>
  );
}

export function PermalinkEmbed(props: {
  link: string;
  association?: Association;
  api: GlobalApi;
  transcluded: number;
  showOurContact?: boolean;
  full?: boolean;
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
          full={props.full}
          showOurContact={props.showOurContact}
        />
      );
  }
}
