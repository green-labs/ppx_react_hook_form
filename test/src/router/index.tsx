import {
  createBrowserRouter,
} from "react-router-dom";
import Root from "../pages/root/Root";
import Basic from "../pages/basic/Basic"
import BasicRes from "../pages/basic_res/Basic.gen"
import SchemaValidation from "../pages/schema_validation/SchemaValidation";
import SchemaValidationRes from "../pages/schema_validation_res/SchemaValidation.gen";
import Controller from "../pages/controller/Controller"
import ControllerRes from "../pages/controller_res/Controller.gen"
import FieldArray from "../pages/field_array/FieldArray";
import FieldArrayRes from "../pages/field_array_res/FieldArray.gen";

const router = createBrowserRouter([
  {
    path: "/",
    element: <Root />
  },
  {
    path: "/basic",
    element: <Basic />
  },
  {
    path: "/basic_res",
    element: <BasicRes />
  },
  {
    path: "/schema_validation",
    element: <SchemaValidation />
  },
  {
    path: "/schema_validation_res",
    element: <SchemaValidationRes />
  },
  {
    path: "/controller",
    element: <Controller />
  },
  {
    path: "/controller_res",
    element: <ControllerRes />
  },
  {
    path: "/field_array",
    element: <FieldArray />
  },
  {
    path: "/field_array_res",
    element: <FieldArrayRes />
  },
])

export default router