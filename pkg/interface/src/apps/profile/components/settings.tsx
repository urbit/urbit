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

type ProfileProps = StoreState & { api: GlobalApi; ship: string };

export default function Profile({
  api,
  launch,
  s3,
  dark,
  hideAvatars,
  hideNicknames,
  background,
}: ProfileProps) {
  return (
    <Col
      backgroundColor="white"
      fontSize={2}
      p={4}
      m={3}
      borderRadius={1}
      maxWidth="300px"
    >
      <Box color="black" fontSize={0} mb={4}>
        Ship Settings
      </Box>
      <DisplayForm
        api={api}
        launch={launch}
        dark={dark}
        hideNicknames={hideNicknames}
        hideAvatars={hideAvatars}
        background={background}
      />

      <S3Form api={api} s3={s3} />

      <SecuritySettings api={api} />
    </Col>
  );
}
