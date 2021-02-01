import React, { useCallback, useState } from "react";
import { Box, Row, Col } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { Invites as IInvites, Associations, Invite, JoinRequests, Groups } from "~/types";
import { resourceAsPath } from "~/logic/lib/util";
import { useHistory } from "react-router-dom";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import InviteItem from "~/views/components/Invite";
import {JoiningStatus} from "./joining";
import {useModal} from "~/logic/lib/useModal";
import {JoinGroup} from "~/views/landscape/components/JoinGroup";

interface InvitesProps {
  api: GlobalApi;
  invites: IInvites;
  groups: Groups;
  associations: Associations;
  pendingJoin: JoinRequests;
}

export function Invites(props: InvitesProps) {
  const { api, invites, pendingJoin } = props;
  const [selected, setSelected] = useState<[string, string, Invite] | undefined>()
  const history = useHistory();
  const waiter = useWaitForProps(props);

  const acceptInvite = (
    app: string,
    uid: string,
    invite: Invite
  ) => async () => {
    setSelected([app, uid, invite]);
    showModal();
  };

  const declineInvite = useCallback(
    (app: string, uid: string) => () => api.invite.decline(app, uid),
    [api]
  );

  const { modal, showModal } = useModal({ modal: () => {
    const [app, uid, invite] = selected!;
    const autojoin = `~${invite.resource.ship}/${invite.resource.name}`;
    return (
    <JoinGroup
      groups={props.groups}
      associations={props.associations}
      api={api}
      autojoin={autojoin}
      inviteUid={uid}
      inviteApp={app}
    />
  )}});



  return (
    <Col
      zIndex={4}
      gapY={2}
      bg="white"
      top="0px"
      position="sticky"
      flexShrink={0}
    >
     {modal}
     { Object
        .keys(props.pendingJoin)
        .map(resource => (
          <JoiningStatus 
            key={resource}
            resource={resource}
            status={pendingJoin[resource]} 
            api={api} />
          ))
        }

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
