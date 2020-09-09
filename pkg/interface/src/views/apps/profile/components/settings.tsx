import React from "react";

import {
  Box,
  Text,
  Button,
  Col,
  Input,
  InputLabel,
  Radio,
  Checkbox,
} from "@tlon/indigo-react";
import * as Yup from "yup";
import { Formik, Form } from "formik";
import _ from "lodash";

import GlobalApi from "../../../api/global";
import { StoreState } from "../../../store/type";
import DisplayForm from "./lib/DisplayForm";
import S3Form from "./lib/S3Form";
import SecuritySettings from "./lib/Security";
import RemoteContentForm from "./lib/RemoteContent";

type ProfileProps = StoreState & { api: GlobalApi; ship: string };

export default function Settings({
  api,
  launch,
  s3,
  dark,
  hideAvatars,
  hideNicknames,
  background,
  remoteContentPolicy
}: ProfileProps) {
  return (
    <Box
      backgroundColor="white"
      fontSize={2}
      display="grid"
      gridTemplateRows="auto"
      gridTemplateColumns="1fr"
      gridRowGap={7}
      p={4}
      maxWidth="400px"
    >
      <DisplayForm
        api={api}
        launch={launch}
        dark={dark}
        hideNicknames={hideNicknames}
        hideAvatars={hideAvatars}
        background={background}
        s3={s3}
      />
      <RemoteContentForm {...{api, remoteContentPolicy}} />
      <S3Form api={api} s3={s3} />
      <SecuritySettings api={api} />
    </Box>
  );
}
