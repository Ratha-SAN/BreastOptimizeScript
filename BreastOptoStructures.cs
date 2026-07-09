// ============================================================================
//  Breast Opto Structures – Varian Eclipse ESAPI single-file plug-in
//
//  Generates the optimisation structures for breast / chest-wall VMAT plans
//  that use a virtual bolus for skin flash (see reference CT image: the
//  virtual bolus is the ring hugging the body outline and the virtual PTV
//  is the PTV extended into that ring).
//
//  Workflow
//  --------
//  WITHOUT physical bolus (base behaviour, unchanged):
//    * VirtualBolus  = ring on the skin, thickness = user input (mm),
//                      limited to the region around the selected PTV,
//                      assigned HU = 0 (so the optimiser sees tissue).
//    * PTV_Virtual   = PTV expanded by (virtual thickness + 2 mm),
//                      cropped to Body + VirtualBolus.
//
//  WITH physical bolus (check-box ticked):
//    * The physical bolus thickness combo box (5 / 10 / 15 / 20 mm) becomes
//      enabled.
//    * The virtual bolus is built exactly like the non-physical option but
//      its thickness = virtual input + physical bolus thickness, and the
//      virtual PTV uses that combined thickness + 2 mm.
//    * Bolus_physical    = copy of the existing BOLUS-type structure in the
//                          set (i.e. the scanned physical bolus). If none
//                          exists, it is built on the skin from the selected
//                          physical thickness instead. Assigned 0 HU
//                          (tissue equivalent).
//    * Bolus_phys_Opt    = new structure = overlap (intersection) between
//                          Bolus_physical and PTV_Virtual. (Named
//                          "Bolus_phys_Opt" rather than "Bolus_physical_Opt"
//                          to stay within Eclipse's 16-character ID limit.)
//    * Body_new          = new structure = union of Body + Bolus_physical +
//                          PTV_Virtual.
//
//  Deploy: copy this .cs file into the Eclipse scripting folder and run it
//  from "Tools > Scripts..." with a plan or structure set open.
// ============================================================================

using System;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using VMS.TPS.Common.Model.API;
using VMS.TPS.Common.Model.Types;

// This script creates / modifies structures, so it must be write-enabled.
[assembly: ESAPIScript(IsWriteable = true)]

namespace VMS.TPS
{
    public class Script
    {
        // ------------------------------------------------------------------
        //  Structure IDs (max 16 characters) – adjust to local convention.
        // ------------------------------------------------------------------
        private const string VirtualBolusId = "VirtualBolus";
        private const string VirtualPtvId = "PTV_Virtual";
        private const string PhysicalBolusId = "Bolus_physical";  // copy of the real (scanned) bolus
        private const string PhysicalBolusOptId = "Bolus_phys_Opt"; // overlap of Bolus_physical and PTV_Virtual
        private const string BodyNewId = "Body_new";               // union of Body + Bolus_physical + PTV_Virtual

        // ------------------------------------------------------------------
        //  Geometry constants (mm).
        // ------------------------------------------------------------------
        private const double VirtualPtvExtraMm = 2.0;   // extra margin for the virtual PTV
        private const double BolusWrapMarginMm = 10.0;  // how far the bolus ring wraps beyond the PTV
        private const double BolusAssignedHu = 0.0;   // HU assigned to virtual + physical(copy) bolus
        private const double MaxEsapiMarginMm = 50.0;  // hard ESAPI limit for SegmentVolume.Margin

        private static readonly double[] PhysicalThicknessOptionsMm = { 5.0, 10.0, 15.0, 20.0 };

        // UI elements
        private ScriptContext _ctx;
        private Window _window;
        private ComboBox _ptvCombo;
        private TextBox _virtualThicknessBox;
        private CheckBox _physicalBolusCheck;
        private ComboBox _physicalThicknessCombo;
        private TextBlock _summaryText;

        public void Execute(ScriptContext context, Window window)
        {
            if (context == null || context.StructureSet == null)
            {
                MessageBox.Show(
                    "No structure set is currently open.\n\n" +
                    "Please open a patient with a structure set (or plan) and run the script again.",
                    "Breast Opto Structures",
                    MessageBoxButton.OK,
                    MessageBoxImage.Warning);
                return;
            }

            _ctx = context;
            _window = window;

            BuildUi(window);
        }

        // ==================================================================
        //  UI
        // ==================================================================
        private void BuildUi(Window window)
        {
            window.Title = "Breast Opto Structures";
            window.Width = 430;
            window.Height = 360;
            window.ResizeMode = ResizeMode.NoResize;

            var panel = new StackPanel { Margin = new Thickness(12) };

            // --- PTV selection -------------------------------------------
            panel.Children.Add(new TextBlock
            {
                Text = "Target PTV:",
                Margin = new Thickness(0, 0, 0, 2)
            });

            _ptvCombo = new ComboBox { Margin = new Thickness(0, 0, 0, 8) };
            foreach (Structure s in _ctx.StructureSet.Structures
                         .Where(s => !s.IsEmpty &&
                                     (s.DicomType == "PTV" ||
                                      s.Id.IndexOf("PTV", StringComparison.OrdinalIgnoreCase) >= 0))
                         .OrderBy(s => s.Id))
            {
                _ptvCombo.Items.Add(s.Id);
            }
            if (_ptvCombo.Items.Count > 0) _ptvCombo.SelectedIndex = 0;
            _ptvCombo.SelectionChanged += (s, e) => UpdateSummary();
            panel.Children.Add(_ptvCombo);

            // --- Virtual bolus thickness ---------------------------------
            panel.Children.Add(new TextBlock
            {
                Text = "Virtual bolus thickness (mm):",
                Margin = new Thickness(0, 0, 0, 2)
            });

            _virtualThicknessBox = new TextBox
            {
                Text = "10",
                Margin = new Thickness(0, 0, 0, 8)
            };
            _virtualThicknessBox.TextChanged += (s, e) => UpdateSummary();
            panel.Children.Add(_virtualThicknessBox);

            // --- Physical bolus option -----------------------------------
            _physicalBolusCheck = new CheckBox
            {
                Content = "Physical bolus is used",
                Margin = new Thickness(0, 4, 0, 6),
                IsChecked = false
            };
            _physicalBolusCheck.Checked += (s, e) => OnPhysicalBolusToggled();
            _physicalBolusCheck.Unchecked += (s, e) => OnPhysicalBolusToggled();
            panel.Children.Add(_physicalBolusCheck);

            panel.Children.Add(new TextBlock
            {
                Text = "Physical bolus thickness (mm):",
                Margin = new Thickness(0, 0, 0, 2)
            });

            _physicalThicknessCombo = new ComboBox
            {
                Margin = new Thickness(0, 0, 0, 8),
                IsEnabled = false           // enabled only when the check-box is ticked
            };
            foreach (double t in PhysicalThicknessOptionsMm)
            {
                _physicalThicknessCombo.Items.Add(t.ToString("0"));
            }
            _physicalThicknessCombo.SelectedIndex = 1; // default 10 mm
            _physicalThicknessCombo.SelectionChanged += (s, e) => UpdateSummary();
            panel.Children.Add(_physicalThicknessCombo);

            // --- Live summary of what will be built ----------------------
            _summaryText = new TextBlock
            {
                TextWrapping = TextWrapping.Wrap,
                Margin = new Thickness(0, 4, 0, 8),
                FontStyle = FontStyles.Italic
            };
            panel.Children.Add(_summaryText);

            // --- Create button -------------------------------------------
            var createButton = new Button
            {
                Content = "Create structures",
                Height = 30,
                Margin = new Thickness(0, 4, 0, 0)
            };
            createButton.Click += (s, e) => OnCreateClicked();
            panel.Children.Add(createButton);

            window.Content = panel;
            UpdateSummary();
        }

        private void OnPhysicalBolusToggled()
        {
            _physicalThicknessCombo.IsEnabled = _physicalBolusCheck.IsChecked == true;
            UpdateSummary();
        }

        private void UpdateSummary()
        {
            if (_summaryText == null) return;

            double virtualThk;
            bool ok = double.TryParse(_virtualThicknessBox.Text, out virtualThk);
            bool usePhysical = _physicalBolusCheck.IsChecked == true;
            double physicalThk = usePhysical ? SelectedPhysicalThickness() : 0.0;

            if (!ok)
            {
                _summaryText.Text = "Enter a numeric virtual bolus thickness.";
                return;
            }

            double total = virtualThk + physicalThk;
            _summaryText.Text = string.Format(
                "{0} thickness = {1:0} mm (virtual {2:0} mm{3}).\n" +
                "{4} = PTV + {5:0} mm, cropped to Body + {0}.{6}",
                VirtualBolusId, total, virtualThk,
                usePhysical ? string.Format(" + physical {0:0} mm", physicalThk) : "",
                VirtualPtvId, total + VirtualPtvExtraMm,
                usePhysical
                    ? string.Format("\n{0} (copy of existing BOLUS, 0 HU), {1} (overlap with {2}) and {3} (Body + {0} + {2}) will also be created.",
                                    PhysicalBolusId, PhysicalBolusOptId, VirtualPtvId, BodyNewId)
                    : "");
        }

        private double SelectedPhysicalThickness()
        {
            return PhysicalThicknessOptionsMm[Math.Max(0, _physicalThicknessCombo.SelectedIndex)];
        }

        // ==================================================================
        //  Structure generation
        // ==================================================================
        private void OnCreateClicked()
        {
            try
            {
                if (_ptvCombo.SelectedItem == null)
                {
                    MessageBox.Show("Please select a PTV.", "Breast Opto Structures");
                    return;
                }

                double virtualThk;
                if (!double.TryParse(_virtualThicknessBox.Text, out virtualThk) ||
                    virtualThk <= 0 || virtualThk > 30)
                {
                    MessageBox.Show("Virtual bolus thickness must be a number between 1 and 30 mm.",
                                    "Breast Opto Structures");
                    return;
                }

                bool usePhysical = _physicalBolusCheck.IsChecked == true;
                double physicalThk = usePhysical ? SelectedPhysicalThickness() : 0.0;
                double totalThk = virtualThk + physicalThk;

                if (totalThk + BolusWrapMarginMm > MaxEsapiMarginMm)
                {
                    MessageBox.Show(string.Format(
                        "Combined bolus thickness ({0:0} mm) is too large – the total expansion must stay below {1:0} mm.",
                        totalThk, MaxEsapiMarginMm - BolusWrapMarginMm),
                        "Breast Opto Structures");
                    return;
                }

                StructureSet ss = _ctx.StructureSet;

                Structure body = ss.Structures.FirstOrDefault(s => s.DicomType == "EXTERNAL")
                                 ?? ss.Structures.FirstOrDefault(s =>
                                        s.Id.Equals("BODY", StringComparison.OrdinalIgnoreCase));
                if (body == null || body.IsEmpty)
                {
                    MessageBox.Show("No BODY / External structure found.", "Breast Opto Structures");
                    return;
                }

                Structure ptv = ss.Structures.First(s => s.Id == (string)_ptvCombo.SelectedItem);

                _ctx.Patient.BeginModifications();

                string report = GenerateStructures(ss, body, ptv, virtualThk, physicalThk, usePhysical);

                MessageBox.Show(report, "Breast Opto Structures – done",
                                MessageBoxButton.OK, MessageBoxImage.Information);
                _window.Close();
            }
            catch (Exception ex)
            {
                MessageBox.Show("Structure generation failed:\n\n" + ex.Message,
                                "Breast Opto Structures",
                                MessageBoxButton.OK, MessageBoxImage.Error);
            }
        }

        private string GenerateStructures(StructureSet ss, Structure body, Structure ptv,
                                          double virtualThk, double physicalThk, bool usePhysical)
        {
            double totalThk = virtualThk + physicalThk;

            SegmentVolume bodyVol = body.SegmentVolume;

            // Region around the PTV used to limit the bolus ring laterally,
            // so the bolus follows the PTV as in the reference image.
            SegmentVolume wrapRegion = ptv.SegmentVolume.Margin(totalThk + BolusWrapMarginMm);

            // --------------------------------------------------------------
            // 1) Virtual bolus: skin ring of (virtual + physical) thickness.
            //    Same construction as the non-physical option – the physical
            //    bolus simply adds to the ring thickness.
            // --------------------------------------------------------------
            SegmentVolume virtualBolusVol = bodyVol.Margin(totalThk)
                                                   .Sub(bodyVol)
                                                   .And(wrapRegion);

            Structure virtualBolus = GetOrCreateStructure(ss, VirtualBolusId, "CONTROL");
            virtualBolus.SegmentVolume = virtualBolusVol;
            TryAssignHu(virtualBolus, BolusAssignedHu);

            // --------------------------------------------------------------
            // 2) Virtual PTV: PTV + (total bolus thickness + 2 mm),
            //    cropped to Body + VirtualBolus (no air).
            // --------------------------------------------------------------
            SegmentVolume virtualPtvVol = ptv.SegmentVolume
                                             .Margin(totalThk + VirtualPtvExtraMm)
                                             .And(bodyVol.Or(virtualBolusVol));

            Structure virtualPtv = GetOrCreateStructure(ss, VirtualPtvId, "PTV");
            virtualPtv.SegmentVolume = virtualPtvVol;

            string report = string.Format(
                "Created / updated:\n" +
                "  {0}  (thickness {1:0} mm, {2:0} HU)\n" +
                "  {3}  (PTV + {4:0} mm, cropped to Body + bolus)\n",
                VirtualBolusId, totalThk, BolusAssignedHu,
                VirtualPtvId, totalThk + VirtualPtvExtraMm);

            // --------------------------------------------------------------
            // 3) Physical bolus (copy of the real bolus), its overlap with
            //    the virtual PTV, and the new Body that includes both.
            // --------------------------------------------------------------
            if (usePhysical)
            {
                // Bolus_physical: copy of the existing BOLUS structure (the
                // scanned physical bolus). If none exists, fall back to
                // building it on the skin from the selected thickness.
                Structure existingBolus = ss.Structures.FirstOrDefault(s =>
                    s.DicomType == "BOLUS" && !s.IsEmpty &&
                    !s.Id.Equals(PhysicalBolusId, StringComparison.OrdinalIgnoreCase));

                SegmentVolume physicalVol = existingBolus != null
                    ? existingBolus.SegmentVolume
                    : bodyVol.Margin(physicalThk).Sub(bodyVol).And(wrapRegion);

                Structure bolusPhysical = GetOrCreateStructure(ss, PhysicalBolusId, "BOLUS");
                bolusPhysical.SegmentVolume = physicalVol;
                TryAssignHu(bolusPhysical, BolusAssignedHu);   // tissue-equivalent

                // Bolus_phys_Opt: overlap (intersection) of Bolus_physical and the virtual PTV.
                Structure bolusPhysOpt = GetOrCreateStructure(ss, PhysicalBolusOptId, "PTV");
                bolusPhysOpt.SegmentVolume = physicalVol.And(virtualPtvVol);

                // Body_new: union of Body + Bolus_physical + the virtual PTV.
                Structure bodyNew = GetOrCreateStructure(ss, BodyNewId, "CONTROL");
                bodyNew.SegmentVolume = bodyVol.Or(physicalVol).Or(virtualPtvVol);

                report += string.Format(
                    "  {0}  ({1}, {2:0} HU, type BOLUS)\n" +
                    "  {3}  (overlap of {0} and {4}, type PTV)\n" +
                    "  {5}  (Body + {0} + {4}, type CONTROL)\n",
                    PhysicalBolusId,
                    existingBolus != null
                        ? "copy of '" + existingBolus.Id + "'"
                        : string.Format("built on skin, {0:0} mm", physicalThk),
                    BolusAssignedHu,
                    PhysicalBolusOptId,
                    VirtualPtvId, BodyNewId);
            }

            return report;
        }

        // ==================================================================
        //  Helpers
        // ==================================================================
        private static Structure GetOrCreateStructure(StructureSet ss, string id, string dicomType)
        {
            Structure existing = ss.Structures.FirstOrDefault(s =>
                s.Id.Equals(id, StringComparison.OrdinalIgnoreCase));
            if (existing != null) return existing;

            if (!ss.CanAddStructure(dicomType, id))
            {
                throw new InvalidOperationException(string.Format(
                    "Cannot add structure '{0}' (type {1}) to this structure set.", id, dicomType));
            }
            return ss.AddStructure(dicomType, id);
        }

        private static void TryAssignHu(Structure structure, double hu)
        {
            try
            {
                structure.SetAssignedHU(hu);
            }
            catch
            {
                // Some structure sets / approval states refuse HU assignment;
                // the geometry is still valid, so keep going and let the user
                // assign the HU manually in Contouring.
                MessageBox.Show(string.Format(
                    "Could not assign {0:0} HU to '{1}'. Please assign it manually in Contouring.",
                    hu, structure.Id), "Breast Opto Structures");
            }
        }
    }
}
