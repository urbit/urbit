import React, { useState, useEffect, useCallback } from "react";
import { useHistory } from "react-router-dom";

import {
  MetadataUpdatePreview,
  Contacts,
  JoinRequests,
  Groups,
  Associations,
} from "@urbit/api";
import { Invite } from "@urbit/api/invite";
import { Text, Icon, Row } from "@tlon/indigo-react";

import { cite, useShowNickname } from "~/logic/lib/util";
import GlobalApi from "~/logic/api/global";
import { resourceFromPath } from "~/logic/lib/group";
import { GroupInvite } from "./Group";
import { InviteSkeleton } from "./InviteSkeleton";
import { JoinSkeleton } from "./JoinSkeleton";
import { useWaitForProps } from "~/logic/lib/useWaitForProps";
import useGroupState from "~/logic/state/group";
import useContactState from "~/logic/state/contact";
import useMetadataState from "~/logic/state/metadata";
import useGraphState from "~/logic/state/graph";
import { useRunIO } from "~/logic/lib/useRunIO";

interface InviteItemProps {
  invite?: Invite;
  resource: string;
  pendingJoin?: string;
  app?: string;
  uid?: string;
  api: GlobalApi;
}

export function useInviteAccept(
  resource: string,
  api: GlobalApi,
  app?: string,
  uid?: string,
  invite?: Invite,
) {
  const { ship, name } = resourceFromPath(resource);
  const history = useHistory();
  const associations = useMetadataState((s) => s.associations);
  const groups = useGroupState((s) => s.groups);
  const graphKeys = useGraphState((s) => s.graphKeys);

  const waiter = useWaitForProps({ associations, graphKeys, groups });
  return useRunIO<void, boolean>(
    async () => {
      if (!(app && invite && uid)) {
        return false;
      }
      if (resource in groups) {
        await api.invite.decline(app, uid);
        return false;
      }

      await api.groups.join(ship, name);
      await api.invite.accept(app, uid);
      await waiter((p) => {
        return (
          (resource in p.groups &&
            resource in (p.associations?.graph ?? {}) &&
            p.graphKeys.has(resource.slice(7))) ||
          resource in (p.associations?.groups ?? {})
        );
      });
      return true;
    },
    (success: boolean) => {
      if (!success) {
        return;
      }
      if (groups?.[resource]?.hidden) {
        const { metadata } = associations.graph[resource];
        if (metadata && "graph" in metadata.config) {
          if (metadata.config.graph === "chat") {
            history.push(
              `/~landscape/messages/resource/${metadata.config.graph}${resource}`
            );
          } else {
            history.push(
              `/~landscape/home/resource/${metadata.config.graph}${resource}`
            );
          }
        } else {
          console.error("unknown metadata: ", metadata);
        }
      } else {
        history.push(`/~landscape${resource}`);
      }
    },
    resource
  );
}

export function InviteItem(props: InviteItemProps) {
  const [preview, setPreview] = useState<MetadataUpdatePreview | null>(null);
  const { pendingJoin, invite, resource, uid, app, api } = props;
  const { name } = resourceFromPath(resource);
  const contacts = useContactState((state) => state.contacts);
  const contact = contacts?.[`~${invite?.ship}`] ?? {};
  const showNickname = useShowNickname(contact);

  const inviteAccept = useInviteAccept(resource, api, app, uid, invite);

  const inviteDecline = useCallback(async () => {
    if (!(app && uid)) {
      return;
    }
    await api.invite.decline(app, uid);
  }, [app, uid]);

  const handlers = { onAccept: inviteAccept, onDecline: inviteDecline };

  useEffect(() => {
    if (!app || app === "groups") {
      (async () => {
        setPreview(await api.metadata.preview(resource));
      })();
      return () => {
        setPreview(null);
      };
    } else {
      return () => {};
    }
  }, [invite]);

  if (pendingJoin?.hidden) {
    return null;
  }

  if (preview) {
    return (
      <GroupInvite
        resource={resource}
        api={api}
        preview={preview}
        invite={invite}
        status={pendingJoin}
        {...handlers}
      />
    );
  } else if (invite && name.startsWith("dm--")) {
    return (
      <InviteSkeleton
        gapY="3"
        {...handlers}
        acceptDesc="Join DM"
        declineDesc="Decline DM"
      >
        <Row py="1" alignItems="center">
          <Icon display="block" color="blue" icon="Bullet" mr="2" />
          <Text
            mr="1"
            mono={!showNickname}
            fontWeight={showNickname ? "500" : "400"}
          >
            {showNickname ? contact?.nickname : cite(`~${invite!.ship}`)}
          </Text>
          <Text mr="1">invited you to a DM</Text>
        </Row>
      </InviteSkeleton>
    );
  } else if (status && name.startsWith("dm--")) {
    return (
      <JoinSkeleton api={api} resource={resource} status={status} gapY="3">
        <Row py="1" alignItems="center">
          <Icon display="block" color="blue" icon="Bullet" mr="2" />
          <Text mr="1">Joining direct message...</Text>
        </Row>
      </JoinSkeleton>
    );
  } else if (invite) {
    return (
      <InviteSkeleton
        acceptDesc="Accept Invite"
        declineDesc="Decline Invite"
        resource={resource}
        {...handlers}
        gapY="3"
      >
        <Row py="1" alignItems="center">
          <Icon display="block" color="blue" icon="Bullet" mr="2" />
          <Text
            mr="1"
            mono={!showNickname}
            fontWeight={showNickname ? "500" : "400"}
          >
            {showNickname ? contact?.nickname : cite(`~${invite!.ship}`)}
          </Text>
          <Text mr="1">
            invited you to ~{invite.resource.ship}/{invite.resource.name}
          </Text>
        </Row>
      </InviteSkeleton>
    );
  } else if (pendingJoin) {
    const [, , ship, name] = resource.split("/");
    return (
      <JoinSkeleton api={api} resource={resource} status={pendingJoin}>
        <Row py="1" alignItems="center">
          <Icon display="block" color="blue" icon="Bullet" mr="2" />
          <Text mr="1">You are joining</Text>
          <Text mono>
            {cite(ship)}/{name}
          </Text>
        </Row>
      </JoinSkeleton>
    );
  }
  return null;
}

export default InviteItem;
