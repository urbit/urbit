import React, { useCallback } from "react";
import _ from "lodash";
import { Invites, Invite } from "~/types";
import { Box, Col, Icon, Row, Text } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { StatelessAsyncButton } from "../StatelessAsyncButton";
import { cite } from "~/logic/lib/util";
import { resourceFromPath } from "~/logic/lib/group";

function appNameToIcon(app: string): any {
  if (app === "link") {
    return "Links";
  }
  if (app === "contacts") {
    return "Groups";
  }
  return _.capitalize(app);
}

function SidebarInvite(props: {
  invite: Invite;
  uid: string;
  appPath: string;
  api: GlobalApi;
}) {
  const { invite, appPath, api, uid } = props;
  const { app } = invite;

  const onAccept = useCallback(async () => {
    if (app === "contacts") {
      const resource = resourceFromPath(invite.path);
      await api.contacts.join(resource);
    }
    await api.invite.accept(appPath, uid);
  }, [api, appPath, uid]);

  const onDecline = useCallback(async () => {
    await api.invite.decline(appPath, uid);
  }, [api, appPath, uid]);

  const path = invite.path.startsWith("/ship/")
    ? invite.path.slice(6)
    : invite.path.slice(1);

  return (
    <Box p={2} width="100%">
      <Col gapY="3" p={2} borderRadius="1" border={1} borderColor="washedGray">
        <Row>
          <Icon color="transparent" stroke="gray" icon={appNameToIcon(app)} />
          <Text ml="2" mono>
            {path}
          </Text>
        </Row>
        <Row width="100%" justifyContent="space-around">
          <StatelessAsyncButton onClick={onAccept} primary>
            Accept
          </StatelessAsyncButton>
          <StatelessAsyncButton onClick={onDecline} destructive>
            Decline
          </StatelessAsyncButton>
        </Row>
      </Col>
    </Box>
  );
}

export function SidebarInvites(props: { invites: Invites; api: GlobalApi }) {
  const { invites, api } = props;

  return (
    <>
      {_.flattenDeep(
        Object.keys(invites).map((appPath) =>
          Object.keys(invites[appPath]).map((uid) => (
            <SidebarInvite
              key={uid}
              uid={uid}
              invite={invites[appPath][uid]}
              api={api}
              appPath={appPath}
            />
          ))
        )
      )}
    </>
  );
}
