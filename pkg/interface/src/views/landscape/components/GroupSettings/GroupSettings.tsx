import React, { useEffect } from "react";
import {
  Box,
  Col,
} from "@tlon/indigo-react";
import { Group } from "~/types/group-update";
import { Association, Associations } from "~/types/metadata-update";
import GlobalApi from "~/logic/api/global";

import { GroupAdminSettings } from "./Admin";
import { GroupPersonalSettings } from "./Personal";
import {GroupNotificationsConfig} from "~/types";
import {GroupChannelSettings} from "./Channels";

interface GroupSettingsProps {
  group: Group;
  association: Association;
  associations: Associations;
  api: GlobalApi;
  notificationsGroupConfig: GroupNotificationsConfig;
}
export function GroupSettings(props: GroupSettingsProps) {
  return (
    <Box height="100%" overflowY="auto">
      <Col maxWidth="384px" p="4" gapY="4">
        <GroupPersonalSettings {...props} />
        <Box borderBottom="1" borderBottomColor="washedGray" />
        <GroupAdminSettings {...props} />
        <Box borderBottom="1" borderBottomColor="washedGray" />
        <GroupChannelSettings {...props} />
      </Col>
    </Box>
  );
}
