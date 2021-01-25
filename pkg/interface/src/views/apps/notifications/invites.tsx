import React, { useCallback } from "react";
import { Box, Row, Col } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { Invites as IInvites, Associations, Invite } from "~/types";
import { resourceAsPath } from "~/logic/lib/util";
import { useHistory } from "react-router-dom";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import InviteItem from "~/views/components/Invite";

interface InvitesProps {
  api: GlobalApi;
  invites: IInvites;
  associations: Associations;
}

export function Invites(props: InvitesProps) {
  const { api, invites } = props;
  const history = useHistory();
  const waiter = useWaitForProps(props);

  const acceptInvite = (
    app: string,
    uid: string,
    invite: Invite
  ) => async () => {
    const resource = {
      ship: `~${invite.resource.ship}`,
      name: invite.resource.name,
    };

    const resourcePath = resourceAsPath(invite.resource);
    if (app === "contacts") {
      await api.contacts.join(resource);
      await waiter((p) => resourcePath in p.associations?.contacts);
      await api.invite.accept(app, uid);
      history.push(`/~landscape${resourcePath}`);
    } else if (app === "graph") {
      await api.invite.accept(app, uid);
      history.push(`/~graph/join${resourcePath}`);
    }
  };

  const declineInvite = useCallback(
    (app: string, uid: string) => () => api.invite.decline(app, uid),
    [api]
  );

  return (
    <Col
      zIndex={4}
      gapY={2}
      bg="white"
      top="0px"
      position="sticky"
      flexShrink={0}
    >
      {Object.keys(invites).reduce((items, appKey) => {
        const app = invites[appKey];
        let appItems = Object.keys(app).map((uid) => {
          const invite = app[uid];
          return (
            <InviteItem
              key={uid}
              invite={invite}
              onAccept={acceptInvite(appKey, uid, invite)}
              onDecline={declineInvite(appKey, uid)}
            />
          );
        });
        return [...items, ...appItems];
      }, [] as JSX.Element[])}
    </Col>
  );
}
