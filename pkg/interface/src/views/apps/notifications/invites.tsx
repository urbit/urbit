import React, { useCallback, useState } from "react";
import _ from 'lodash';
import { Box, Row, Col } from "@tlon/indigo-react";
import GlobalApi from "~/logic/api/global";
import { Invites as IInvites, Associations, Invite, JoinRequests, Groups, Contacts, AppInvites, JoinProgress } from "~/types";
import { resourceAsPath, alphabeticalOrder } from "~/logic/lib/util";
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
  contacts: Contacts;
  associations: Associations;
  pendingJoin: JoinRequests;
}

interface InviteRef {
  uid: string;
  app: string
  invite: Invite;
}

export function Invites(props: InvitesProps) {
  const { api, invites, pendingJoin } = props;
  const [selected, setSelected] = useState<[string, string, Invite] | undefined>()

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

  const inviteArr: InviteRef[] = _.reduce(invites, (acc: InviteRef[], val: AppInvites, app: string) => {
    const appInvites = _.reduce(val, (invs: InviteRef[], invite: Invite, uid: string) => {
      return [...invs, { invite, uid, app }];

    }, []);
    return [...acc, ...appInvites];
  }, []);

  const invitesAndStatus: { [rid: string]: JoinProgress | InviteRef } = 
    {..._.keyBy(inviteArr, ({ invite }) => resourceAsPath(invite.resource)), ...props.pendingJoin };



  return (
    <Col
      zIndex={4}
      gapY={2}
      bg="white"
      top="0px"
      position="sticky"
      flexShrink={0}
    >
     { Object
         .keys(invitesAndStatus)
         .sort(alphabeticalOrder)
         .map(resource => {
           const inviteOrStatus = invitesAndStatus[resource];
          if(typeof inviteOrStatus === 'string') {
           return (
             <InviteItem
               key={resource}
               contacts={props.contacts}
               groups={props.groups}
               associations={props.associations}
               resource={resource}
               pendingJoin={pendingJoin} 
               api={api} />
          )

        } else {
          const { app, uid, invite } = inviteOrStatus;
          console.log(inviteOrStatus);
          return (
            <InviteItem
              key={resource}
              api={api}
              invite={invite}
              app={app}
              uid={uid}
              pendingJoin={pendingJoin} 
              resource={resource}
              contacts={props.contacts}
              groups={props.groups}
              associations={props.associations}
            />
            )
        }
     })}
    </Col>
  );
}
