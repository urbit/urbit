import React, { useEffect } from "react";
import { AsyncButton } from "~/views/components/AsyncButton";
import * as Yup from "yup";
import {
  Box,
  ManagedTextInputField as Input,
  ManagedToggleSwitchField as Checkbox,
  Col,
  Label,
  Button,
} from "@tlon/indigo-react";
import { Formik, Form, useFormikContext, FormikHelpers } from "formik";
import { FormError } from "~/views/components/FormError";
import { Group, GroupPolicy } from "~/types/group-update";
import { Enc } from "~/types/noun";
import { Association } from "~/types/metadata-update";
import GlobalApi from "~/logic/api/global";
import { resourceFromPath, roleForShip } from "~/logic/lib/group";
import { StatelessAsyncButton } from "~/views/components/StatelessAsyncButton";
import { ColorInput } from "~/views/components/ColorInput";
import { useHistory } from "react-router-dom";

import { uxToHex } from "~/logic/lib/util";
import { GroupAdminSettings } from "./Admin";
import { GroupPersonalSettings } from "./Personal";
import {GroupNotificationsConfig} from "~/types";

interface GroupSettingsProps {
  group: Group;
  association: Association;
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
      </Col>
    </Box>
  );
}
