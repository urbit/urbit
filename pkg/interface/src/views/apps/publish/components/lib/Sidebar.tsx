import React from "react";
import { Box, Text, Col } from "@tlon/indigo-react";
import { Link } from "react-router-dom";
import SidebarInvite from "~/views/components/SidebarInvite";
import { Welcome } from "./Welcome";
import { GroupItem } from "./GroupItem";
import { alphabetiseAssociations } from "~/logic/lib/util";
import _ from "lodash";

export function Sidebar(props: any) {
  const sidebarInvites = !(props.invites && props.invites["/publish"])
    ? null
    : Object.keys(props.invites["/publish"]).map((uid) => {
        return (
          <SidebarInvite
            key={uid}
            invite={props.invites["/publish"][uid]}
            onAccept={() => props.api.invite.accept("/publish", uid)}
            onDecline={() => props.api.invite.decline("/publish", uid)}
          />
        );
      });
  const associations =
    props.associations && "contacts" in props.associations
      ? alphabetiseAssociations(props.associations.contacts)
      : {};
  const appAssociations =
    props.associations && "graph" in props.associations
      ? props.associations.graph
      : {};

  const groups = props.groups || {};

  const groupedItems = _.chain(Array.from(props.graphKeys))
    .reduce((acc, path) => {
      const appPath = `/ship/~${path}`;
      return appPath in appAssociations
        ? [...acc, appAssociations[appPath]]
        : acc;
    }, [] as any[])
    .groupBy((association) =>
      groups?.[association["group-path"]]?.hidden
        ? "unmanaged"
        : association["group-path"]
    )
    .map((appPaths, groupPath) => (
      <GroupItem
        key={groupPath}
        groupAssociation={groupPath !== "unmanaged" && associations[groupPath]}
        appAssociations={appAssociations}
        appPaths={_.map(appPaths, 'app-path')}
        path={props.path}
      />
    ))
    .value();

  const display = props.hidden ? ['none', 'block'] : 'block';

  return (
    <Col
      borderRight={[0, 1]}
      borderRightColor={["washedGray", "washedGray"]}
      height="100%"
      pt={[3, 0]}
      overflowY="auto"
      display={display}
      flexShrink={0}
      width={["auto", "250px"]}
    >
      <Box>
        <Link to="/~publish/new" className="green2 pa4 f9 dib">
          <Box color="green">New Notebook</Box>
        </Link>
      </Box>
      <Box overflowY="auto" pb={1}>
        <Welcome mx={2} />
        {sidebarInvites}
        {groupedItems}
      </Box>
    </Col>
  );
}

export default Sidebar;
